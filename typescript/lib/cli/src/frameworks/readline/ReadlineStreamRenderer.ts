/**
 * Readline Stream Renderer implementation
 *
 * Implements IStreamRenderer interface using character-by-character output
 * and ANSI escape sequences for streaming animations.
 */

import type { IStreamRenderer } from '../../abstractions/IUIComponent'

/**
 * Streaming configuration
 */
interface StreamingConfig {
  throttleMs: number
  showCursor: boolean
  bufferSize: number
  colorCode?: number
  prefix?: string
  suffix?: string
}

/**
 * Streaming state
 */
interface StreamingState {
  isActive: boolean
  totalContent: string
  currentPosition: number
  startTime: Date | null
  buffer: string[]
  completedChunks: number
  cancelled: boolean
}

/**
 * Default configuration
 */
const DEFAULT_CONFIG: StreamingConfig = {
  throttleMs: 0, // No throttling by default
  showCursor: true,
  bufferSize: 1000,
  colorCode: 37, // White
  prefix: '',
  suffix: '',
}

/**
 * Event callback types
 */
type StreamCompleteCallback = (content: string) => void
type StreamCancelCallback = () => void

/**
 * Readline stream renderer for character-by-character output
 * Provides smooth streaming with configurable throttling and animations
 */
export class ReadlineStreamRenderer implements IStreamRenderer {
  private config: StreamingConfig
  private state: StreamingState
  private visible = false
  private isDestroyed = false
  private streamingTimer: NodeJS.Timeout | null = null

  // Event callbacks
  private onCompleteCallbacks: StreamCompleteCallback[] = []
  private onCancelCallbacks: StreamCancelCallback[] = []

  constructor(config: Partial<StreamingConfig> = {}) {
    this.config = { ...DEFAULT_CONFIG, ...config }
    this.state = this.createInitialState()
  }

  /**
   * Show the component
   */
  show(): void {
    if (this.isDestroyed) {
      return
    }

    this.visible = true
  }

  /**
   * Hide the component
   */
  hide(): void {
    if (this.isDestroyed) {
      return
    }

    this.stopStreaming()
    this.visible = false
  }

  /**
   * Check if component is visible
   */
  isVisible(): boolean {
    return this.visible && !this.isDestroyed
  }

  /**
   * Update component configuration
   */
  updateConfig(config: Partial<StreamingConfig>): void {
    if (this.isDestroyed) {
      return
    }

    this.config = { ...this.config, ...config }
  }

  /**
   * Clean up component resources
   */
  destroy(): void {
    if (this.isDestroyed) {
      return
    }

    this.cancel()
    this.onCompleteCallbacks = []
    this.onCancelCallbacks = []
    this.isDestroyed = true
  }

  /**
   * Start streaming output
   */
  startStreaming(): void {
    if (this.isDestroyed || this.state.isActive) {
      return
    }

    this.state = {
      ...this.createInitialState(),
      isActive: true,
      startTime: new Date(),
    }

    this.visible = true

    // Show cursor if enabled
    if (this.config.showCursor) {
      this.showStreamingCursor()
    }

    // Start processing buffer
    this.processBuffer()
  }

  /**
   * Add a chunk of content to the stream
   */
  addChunk(content: string): void {
    if (this.isDestroyed || !this.state.isActive || this.state.cancelled) {
      return
    }

    // Add to buffer
    this.state.buffer.push(content)

    // Trim buffer if it gets too large
    if (this.state.buffer.length > this.config.bufferSize) {
      this.state.buffer = this.state.buffer.slice(-this.config.bufferSize)
    }

    // Process buffer if not already processing
    if (!this.streamingTimer) {
      this.processBuffer()
    }
  }

  /**
   * Complete the streaming process
   */
  complete(message?: string): void {
    if (this.isDestroyed || !this.state.isActive) {
      return
    }

    // Process any remaining buffer content
    this.flushBuffer()

    // Add completion message if provided
    if (message) {
      this.writeContent(`\n${message}\n`)
    } else {
      this.writeContent('\n')
    }

    // Hide cursor
    if (this.config.showCursor) {
      this.hideStreamingCursor()
    }

    // Update state
    this.state.isActive = false
    const finalContent = this.state.totalContent

    // Notify completion callbacks
    this.onCompleteCallbacks.forEach((callback) => {
      try {
        callback(finalContent)
      } catch (error) {
        console.error('Error in stream complete callback:', error)
      }
    })

    this.cleanup()
  }

  /**
   * Cancel streaming
   */
  cancel(): void {
    if (this.isDestroyed || !this.state.isActive) {
      return
    }

    this.state.cancelled = true
    this.state.isActive = false

    // Stop processing
    this.stopStreaming()

    // Add cancellation indicator
    this.writeContent('\n[Streaming cancelled]\n')

    // Hide cursor
    if (this.config.showCursor) {
      this.hideStreamingCursor()
    }

    // Notify cancellation callbacks
    this.onCancelCallbacks.forEach((callback) => {
      try {
        callback()
      } catch (error) {
        console.error('Error in stream cancel callback:', error)
      }
    })

    this.cleanup()
  }

  /**
   * Check if currently streaming
   */
  isStreaming(): boolean {
    return this.state.isActive && !this.state.cancelled
  }

  /**
   * Get current streaming state
   */
  getStreamingState(): StreamingState {
    return { ...this.state }
  }

  /**
   * Set up streaming event handlers
   */
  onStreamingComplete(callback: StreamCompleteCallback): void {
    if (this.isDestroyed) {
      return
    }

    this.onCompleteCallbacks.push(callback)
  }

  onStreamingCancelled(callback: StreamCancelCallback): void {
    if (this.isDestroyed) {
      return
    }

    this.onCancelCallbacks.push(callback)
  }

  /**
   * Remove specific callbacks
   */
  removeCompleteCallback(callback: StreamCompleteCallback): void {
    const index = this.onCompleteCallbacks.indexOf(callback)
    if (index > -1) {
      this.onCompleteCallbacks.splice(index, 1)
    }
  }

  removeCancelCallback(callback: StreamCancelCallback): void {
    const index = this.onCancelCallbacks.indexOf(callback)
    if (index > -1) {
      this.onCancelCallbacks.splice(index, 1)
    }
  }

  // Private methods

  private createInitialState(): StreamingState {
    return {
      isActive: false,
      totalContent: '',
      currentPosition: 0,
      startTime: null,
      buffer: [],
      completedChunks: 0,
      cancelled: false,
    }
  }

  private processBuffer(): void {
    if (
      this.streamingTimer ||
      !this.state.isActive ||
      this.state.cancelled ||
      this.state.buffer.length === 0
    ) {
      return
    }

    const chunk = this.state.buffer.shift()
    if (!chunk) {
      return
    }

    if (this.config.throttleMs > 0) {
      // Character-by-character streaming with throttling
      this.streamChunkSlowly(chunk)
    } else {
      // Immediate output
      this.writeContent(chunk)
      this.state.completedChunks++

      // Continue processing buffer
      if (this.state.buffer.length > 0) {
        setImmediate(() => this.processBuffer())
      }
    }
  }

  private streamChunkSlowly(chunk: string): void {
    if (this.streamingTimer) {
      return
    }

    let position = 0

    this.streamingTimer = setInterval(() => {
      if (this.state.cancelled || !this.state.isActive) {
        this.stopStreaming()
        return
      }

      if (position >= chunk.length) {
        this.stopStreaming()
        this.state.completedChunks++

        // Continue with next chunk
        if (this.state.buffer.length > 0) {
          this.processBuffer()
        }
        return
      }

      const char = chunk[position]
      this.writeContent(char)
      position++
    }, this.config.throttleMs)
  }

  private flushBuffer(): void {
    // Output all remaining buffer content immediately
    while (this.state.buffer.length > 0) {
      const chunk = this.state.buffer.shift()
      if (chunk) {
        this.writeContent(chunk)
      }
    }
  }

  private writeContent(content: string): void {
    if (!this.visible || this.isDestroyed) {
      return
    }

    // Add prefix/suffix if configured
    let output = content
    if (this.config.prefix && this.state.totalContent === '') {
      output = this.config.prefix + output
    }

    // Apply color if configured
    if (this.config.colorCode && this.supportsColor()) {
      output = this.colorize(output, this.config.colorCode)
    }

    // Write to stdout
    try {
      process.stdout.write(output)
      this.state.totalContent += content
      this.state.currentPosition += content.length
    } catch (error) {
      console.error('ReadlineStreamRenderer: Write failed:', error)
    }
  }

  private showStreamingCursor(): void {
    // Show a subtle streaming indicator
    process.stdout.write('\x1b[?25h') // Show cursor
  }

  private hideStreamingCursor(): void {
    process.stdout.write('\x1b[?25h') // Ensure cursor is visible
  }

  private stopStreaming(): void {
    if (this.streamingTimer) {
      clearInterval(this.streamingTimer)
      this.streamingTimer = null
    }
  }

  private cleanup(): void {
    this.stopStreaming()

    if (this.config.suffix && this.state.totalContent !== '') {
      this.writeContent(this.config.suffix)
    }
  }

  private colorize(text: string, colorCode: number): string {
    return `\x1b[${colorCode}m${text}\x1b[0m`
  }

  private supportsColor(): boolean {
    const { env } = process

    if (env.FORCE_COLOR && env.FORCE_COLOR !== '0') {
      return true
    }

    if (env.NO_COLOR || env.NODE_DISABLE_COLORS) {
      return false
    }

    return process.stdout.isTTY
  }

  /**
   * Get streaming statistics
   */
  getStats(): {
    isActive: boolean
    totalChunks: number
    completedChunks: number
    bufferSize: number
    contentLength: number
    elapsedTime?: number
    charactersPerSecond?: number
  } {
    const stats = {
      isActive: this.state.isActive,
      totalChunks: this.state.completedChunks + this.state.buffer.length,
      completedChunks: this.state.completedChunks,
      bufferSize: this.state.buffer.length,
      contentLength: this.state.totalContent.length,
    }

    if (this.state.startTime && this.state.isActive) {
      const elapsedMs = Date.now() - this.state.startTime.getTime()
      const elapsedSeconds = elapsedMs / 1000

      return {
        ...stats,
        elapsedTime: elapsedMs,
        charactersPerSecond:
          elapsedSeconds > 0 ? this.state.totalContent.length / elapsedSeconds : 0,
      }
    }

    return stats
  }

  /**
   * Utility method to estimate streaming duration
   */
  estimateDuration(content: string): number {
    if (this.config.throttleMs <= 0) {
      return 0
    }

    return content.length * this.config.throttleMs
  }

  /**
   * Clear current streaming content
   */
  clearStreamContent(): void {
    if (this.isDestroyed || this.state.isActive) {
      return
    }

    // Move cursor up and clear lines based on content
    const lines = this.state.totalContent.split('\n').length

    for (let i = 0; i < lines; i++) {
      process.stdout.write('\x1b[1A') // Move up 1 line
      process.stdout.write('\r\x1b[K') // Clear line
    }

    this.state = this.createInitialState()
  }
}
