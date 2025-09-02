/**
 * Readline Progress Renderer implementation
 *
 * Implements IProgressRenderer interface using ASCII art progress bars
 * and ANSI escape sequences for animations.
 */

import type { IProgressRenderer, ProgressConfig } from '../../abstractions/IUIComponent'

/**
 * Progress display configuration
 */
interface ReadlineProgressConfig extends ProgressConfig {
  barLength?: number
  barFilled?: string
  barEmpty?: string
  spinnerFrames?: string[]
  colorCode?: number
  showTime?: boolean
}

/**
 * Default configuration
 */
const DEFAULT_CONFIG: Required<ReadlineProgressConfig> = {
  phase: 'processing',
  progress: 0,
  details: '',
  animated: true,
  showPercentage: true,
  showElapsed: true,
  barLength: 20,
  barFilled: '█',
  barEmpty: '░',
  spinnerFrames: ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'],
  colorCode: 32, // Green
  showTime: true,
}

/**
 * Readline progress renderer using ASCII art and ANSI escape sequences
 * Zero external dependencies - uses only process.stdout
 */
export class ReadlineProgressRenderer implements IProgressRenderer {
  private config: Required<ReadlineProgressConfig>
  private currentState: ProgressConfig | null = null
  private visible = false
  private isDestroyed = false
  private animationTimer: NodeJS.Timeout | null = null
  private animationFrame = 0
  private startTime: Date | null = null

  constructor(config: Partial<ReadlineProgressConfig> = {}) {
    this.config = { ...DEFAULT_CONFIG, ...config }
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

    this.stopAnimation()
    this.clearCurrentLine()
    this.visible = false
    this.currentState = null
    this.startTime = null
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
  updateConfig(config: Partial<ReadlineProgressConfig>): void {
    if (this.isDestroyed) {
      return
    }

    this.config = { ...this.config, ...config }

    // Re-render if visible
    if (this.visible && this.currentState) {
      this.render()
    }
  }

  /**
   * Clean up component resources
   */
  destroy(): void {
    if (this.isDestroyed) {
      return
    }

    this.hide()
    this.isDestroyed = true
  }

  /**
   * Start showing progress
   */
  start(config?: ProgressConfig): void {
    if (this.isDestroyed) {
      return
    }

    this.currentState = {
      ...this.config,
      ...config,
    }

    this.startTime = new Date()
    this.visible = true

    // Start animation if enabled
    if (this.config.animated) {
      this.startAnimation()
    }

    this.render()
  }

  /**
   * Update progress state
   */
  updateProgress(progress: number, phase?: string, details?: string): void {
    if (this.isDestroyed || !this.currentState) {
      return
    }

    this.currentState = {
      ...this.currentState,
      progress: Math.max(0, Math.min(1, progress)),
      phase: phase || this.currentState.phase,
      details: details || this.currentState.details,
    }

    this.render()
  }

  /**
   * Complete progress and optionally show final message
   */
  complete(message?: string): void {
    if (this.isDestroyed) {
      return
    }

    this.updateProgress(1, 'complete', message)

    // Show completed state briefly
    setTimeout(() => {
      this.hide()
    }, 1000)
  }

  /**
   * Hide progress and prepare for content replacement
   */
  hideAndReplace(): void {
    if (this.isDestroyed) {
      return
    }

    this.stopAnimation()

    // Clear current line and move up to clear interfering content
    this.clearCurrentLine()

    // Try to clear previous lines that might contain debug output
    for (let i = 0; i < 3; i++) {
      process.stdout.write('\x1b[1A') // Move up 1 line
      this.clearCurrentLine()
    }

    this.visible = false
    this.currentState = null
  }

  /**
   * Get current progress state
   */
  getCurrentState(): ProgressConfig | null {
    return this.currentState ? { ...this.currentState } : null
  }

  /**
   * Check if progress is currently showing
   */
  isShowing(): boolean {
    return this.visible
  }

  // Private methods

  private render(): void {
    if (!this.visible || !this.currentState || this.isDestroyed) {
      return
    }

    const parts: string[] = []

    // Progress bar
    const bar = this.renderProgressBar(this.currentState.progress || 0)
    parts.push(bar)

    // Percentage
    if (this.config.showPercentage) {
      const percentage = `${Math.round((this.currentState.progress || 0) * 100)}%`
      parts.push(this.colorize(percentage, 36)) // Cyan
    }

    // Spinner and phase
    if (this.currentState.phase) {
      let phaseText = this.currentState.phase

      if (this.config.animated && this.currentState.progress !== 1) {
        const spinner = this.getCurrentSpinner()
        phaseText = `${spinner} ${phaseText}`
      }

      parts.push(this.colorize(phaseText, 33)) // Yellow
    }

    // Details
    if (this.currentState.details) {
      parts.push(`- ${this.currentState.details}`)
    }

    // Elapsed time
    if (this.config.showElapsed && this.startTime) {
      const elapsed = this.formatElapsed(Date.now() - this.startTime.getTime())
      parts.push(this.colorize(`(${elapsed})`, 34)) // Blue
    }

    // Clear line and render
    this.clearCurrentLine()
    process.stdout.write(parts.join(' '))
  }

  private renderProgressBar(progress: number): string {
    const filled = Math.round(this.config.barLength * Math.min(1, Math.max(0, progress)))
    const empty = this.config.barLength - filled

    const filledBar = this.config.barFilled.repeat(filled)
    const emptyBar = this.config.barEmpty.repeat(empty)

    const bar = `[${filledBar}${emptyBar}]`

    return this.colorize(bar, this.config.colorCode)
  }

  private getCurrentSpinner(): string {
    const frames = this.config.spinnerFrames
    return frames[this.animationFrame % frames.length] || '⠋'
  }

  private startAnimation(): void {
    if (this.animationTimer || this.isDestroyed) {
      return
    }

    this.animationTimer = setInterval(() => {
      this.animationFrame++
      this.render()
    }, 100) // Update every 100ms
  }

  private stopAnimation(): void {
    if (this.animationTimer) {
      clearInterval(this.animationTimer)
      this.animationTimer = null
      this.animationFrame = 0
    }
  }

  private clearCurrentLine(): void {
    process.stdout.write('\r\x1b[K')
  }

  private colorize(text: string, colorCode: number): string {
    if (this.supportsColor()) {
      return `\x1b[${colorCode}m${text}\x1b[0m`
    }
    return text
  }

  private supportsColor(): boolean {
    // Simple color detection
    const { env } = process

    if (env.FORCE_COLOR && env.FORCE_COLOR !== '0') {
      return true
    }

    if (env.NO_COLOR || env.NODE_DISABLE_COLORS) {
      return false
    }

    return process.stdout.isTTY
  }

  private formatElapsed(ms: number): string {
    const seconds = Math.floor(ms / 1000)

    if (seconds < 60) {
      return `${seconds}s`
    }

    const minutes = Math.floor(seconds / 60)
    const remainingSeconds = seconds % 60

    if (minutes < 60) {
      return `${minutes}m ${remainingSeconds}s`
    }

    const hours = Math.floor(minutes / 60)
    const remainingMinutes = minutes % 60

    return `${hours}h ${remainingMinutes}m ${remainingSeconds}s`
  }

  /**
   * Create a simple progress bar utility
   */
  static createSimpleBar(progress: number, length = 20, filled = '█', empty = '░'): string {
    const filledLength = Math.round(length * Math.min(1, Math.max(0, progress)))
    const emptyLength = length - filledLength

    return `[${filled.repeat(filledLength)}${empty.repeat(emptyLength)}]`
  }

  /**
   * Get progress statistics
   */
  getStats(): {
    isVisible: boolean
    currentProgress: number
    elapsedTime?: number
    animationFrame: number
  } {
    const stats = {
      isVisible: this.visible,
      currentProgress: this.currentState?.progress || 0,
      animationFrame: this.animationFrame,
    }

    if (this.startTime) {
      return {
        ...stats,
        elapsedTime: Date.now() - this.startTime.getTime(),
      }
    }

    return stats
  }
}
