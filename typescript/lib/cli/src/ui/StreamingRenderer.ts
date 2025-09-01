/**
 * Streaming Renderer Component
 *
 * Handles real-time rendering of streaming content:
 * - Character-by-character text streaming
 * - Word wrapping and terminal formatting
 * - Progress indicators during streaming
 * - Cancellation visual feedback
 */

import { Colors, Terminal } from '../keyboard/KeyboardUtils'

export interface StreamingConfig {
  wordWrap: boolean
  showCursor: boolean
  streamingSpeed: number // Characters per second (0 = no throttling)
  maxWidth?: number // Max line width (undefined = terminal width)
  colors: {
    text: number
    cursor: number
    metadata: number
  }
}

export interface StreamChunk {
  content: string
  type: 'text' | 'metadata' | 'complete' | 'error'
  timestamp?: number
}

/**
 * Renders streaming content in real-time
 */
export class StreamingRenderer {
  private config: StreamingConfig
  private isStreaming = false
  private currentContent = ''
  private currentLine = ''
  private cursorVisible = false
  private cursorInterval: NodeJS.Timeout | null = null
  private streamBuffer: string[] = []
  private streamingTimeout: NodeJS.Timeout | null = null

  constructor(config: Partial<StreamingConfig> = {}) {
    this.config = {
      wordWrap: true,
      showCursor: true,
      streamingSpeed: 0, // No throttling by default
      colors: {
        text: Colors.FG_WHITE,
        cursor: Colors.FG_CYAN,
        metadata: Colors.FG_BLUE,
      },
      ...config,
    }
  }

  /**
   * Start streaming mode
   */
  startStreaming(): void {
    if (this.isStreaming) return

    this.isStreaming = true
    this.currentContent = ''
    this.currentLine = ''

    if (this.config.showCursor) {
      this.startCursor()
    }

    // v-0.6.1: Event emission removed - pure message-driven
  }

  /**
   * Add content to the stream
   */
  addChunk(chunk: StreamChunk): void {
    if (!this.isStreaming) {
      this.startStreaming()
    }

    switch (chunk.type) {
      case 'text':
        this.addTextContent(chunk.content)
        break
      case 'metadata':
        this.addMetadata(chunk.content)
        break
      case 'complete':
        this.complete(chunk.content)
        break
      case 'error':
        this.showError(chunk.content)
        break
    }
  }

  /**
   * Add text content with optional throttling
   */
  private addTextContent(content: string): void {
    if (this.config.streamingSpeed > 0) {
      // Throttled streaming
      for (const char of content) {
        this.streamBuffer.push(char)
      }
      this.processStreamBuffer()
    } else {
      // Immediate streaming
      this.renderText(content)
    }
  }

  /**
   * Process throttled stream buffer
   */
  private processStreamBuffer(): void {
    if (this.streamBuffer.length === 0 || this.streamingTimeout) {
      return
    }

    const char = this.streamBuffer.shift()!
    this.renderText(char)

    if (this.streamBuffer.length > 0) {
      const delay = 1000 / this.config.streamingSpeed
      this.streamingTimeout = setTimeout(() => {
        this.streamingTimeout = null
        this.processStreamBuffer()
      }, delay)
    }
  }

  /**
   * Render text content
   */
  private renderText(content: string): void {
    Terminal.color(this.config.colors.text)

    for (const char of content) {
      if (char === '\n') {
        this.newLine()
      } else {
        this.addCharacter(char)
      }
    }

    Terminal.reset()
    this.updateCursor()
  }

  /**
   * Add a single character with word wrapping
   */
  private addCharacter(char: string): void {
    const terminalWidth = this.getMaxWidth()

    // Check if we need to wrap
    if (this.config.wordWrap && this.currentLine.length >= terminalWidth) {
      // Try to wrap at word boundary
      const lastSpace = this.currentLine.lastIndexOf(' ')
      if (lastSpace > terminalWidth * 0.7) {
        // Wrap at word boundary
        const wrappedPart = this.currentLine.substring(lastSpace + 1)
        this.currentLine = this.currentLine.substring(0, lastSpace)
        this.newLine()
        this.currentLine = wrappedPart
      } else {
        // Force wrap
        this.newLine()
      }
    }

    this.currentLine += char
    process.stdout.write(char)
    this.currentContent += char
  }

  /**
   * Move to new line
   */
  private newLine(): void {
    process.stdout.write('\n')
    this.currentContent += '\n'
    this.currentLine = ''
  }

  /**
   * Add metadata information
   */
  private addMetadata(metadata: string): void {
    Terminal.color(this.config.colors.metadata)
    process.stdout.write(`\n[${metadata}]`)
    Terminal.reset()
    this.newLine()
  }

  /**
   * Show error during streaming
   */
  private showError(error: string): void {
    this.stopCursor()

    Terminal.color(Colors.FG_RED)
    process.stdout.write(`\n❌ Error: ${error}\n`)
    Terminal.reset()

    this.isStreaming = false
    this.clearStreamBuffer()

    // v-0.6.1: Event emission removed - pure message-driven
  }

  /**
   * Complete streaming
   */
  complete(finalMessage?: string): void {
    if (!this.isStreaming) return

    this.stopCursor()

    if (finalMessage) {
      this.addMetadata(`Complete: ${finalMessage}`)
    }

    this.isStreaming = false
    this.clearStreamBuffer()

    // v-0.6.1: Event emission removed - pure message-driven
  }

  /**
   * Cancel streaming
   */
  cancel(): void {
    if (!this.isStreaming) return

    this.stopCursor()
    this.clearStreamBuffer()

    Terminal.color(Colors.FG_YELLOW)
    process.stdout.write('\n⚠️  Streaming cancelled\n')
    Terminal.reset()

    this.isStreaming = false

    // v-0.6.1: Event emission removed - pure message-driven
  }

  /**
   * Start cursor blinking
   */
  private startCursor(): void {
    if (this.cursorInterval) return

    this.cursorVisible = true
    this.renderCursor()

    this.cursorInterval = setInterval(() => {
      this.cursorVisible = !this.cursorVisible
      this.renderCursor()
    }, 500)
  }

  /**
   * Stop cursor blinking
   */
  private stopCursor(): void {
    if (this.cursorInterval) {
      clearInterval(this.cursorInterval)
      this.cursorInterval = null
    }

    // Hide cursor
    if (this.cursorVisible) {
      this.eraseCursor()
      this.cursorVisible = false
    }
  }

  /**
   * Render cursor
   */
  private renderCursor(): void {
    Terminal.saveCursor()

    if (this.cursorVisible) {
      Terminal.color(this.config.colors.cursor)
      process.stdout.write('▋')
      Terminal.reset()
    } else {
      process.stdout.write(' ')
    }

    Terminal.restoreCursor()
  }

  /**
   * Update cursor position
   */
  private updateCursor(): void {
    if (this.config.showCursor && this.cursorVisible) {
      Terminal.saveCursor()
      Terminal.color(this.config.colors.cursor)
      process.stdout.write('▋')
      Terminal.reset()
      Terminal.restoreCursor()
    }
  }

  /**
   * Erase cursor
   */
  private eraseCursor(): void {
    Terminal.saveCursor()
    process.stdout.write(' ')
    Terminal.restoreCursor()
  }

  /**
   * Get maximum line width
   */
  private getMaxWidth(): number {
    return this.config.maxWidth || Terminal.getDimensions().width - 2
  }

  /**
   * Clear stream buffer
   */
  private clearStreamBuffer(): void {
    this.streamBuffer = []
    if (this.streamingTimeout) {
      clearTimeout(this.streamingTimeout)
      this.streamingTimeout = null
    }
  }

  /**
   * Get current streaming state
   */
  getState(): {
    isStreaming: boolean
    content: string
    bufferLength: number
  } {
    return {
      isStreaming: this.isStreaming,
      content: this.currentContent,
      bufferLength: this.streamBuffer.length,
    }
  }

  /**
   * Update configuration
   */
  updateConfig(newConfig: Partial<StreamingConfig>): void {
    this.config = { ...this.config, ...newConfig }
  }

  // v-0.6.1: Custom event emitter functionality completely removed - pure message-driven

  /**
   * Clean up resources
   */
  destroy(): void {
    this.stopCursor()
    this.clearStreamBuffer()
    // v-0.6.1: No listeners to clean up - pure message-driven
  }
}

/**
 * Utility function to create a streaming renderer
 */
export function createStreamingRenderer(config?: Partial<StreamingConfig>): StreamingRenderer {
  return new StreamingRenderer(config)
}

/**
 * Utility function for word wrapping
 */
export function wrapText(text: string, maxWidth: number): string[] {
  const words = text.split(' ')
  const lines: string[] = []
  let currentLine = ''

  for (const word of words) {
    if (currentLine.length + word.length + 1 <= maxWidth) {
      currentLine += (currentLine ? ' ' : '') + word
    } else {
      if (currentLine) lines.push(currentLine)
      currentLine = word
    }
  }

  if (currentLine) lines.push(currentLine)

  return lines
}
