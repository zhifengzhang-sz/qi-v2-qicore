/**
 * Progress Display Component
 *
 * Provides visual progress indicators for CLI operations:
 * - Progress bars with percentage
 * - Phase indicators
 * - Animated spinners
 * - Time tracking
 */

import { Colors, Terminal } from '../keyboard/KeyboardUtils'

export interface ProgressState {
  phase: string
  progress: number // 0-1
  details?: string
  startTime?: number
  message?: string
}

export interface ProgressConfig {
  barLength: number
  showPercentage: boolean
  showPhase: boolean
  showDetails: boolean
  showElapsed: boolean
  animated: boolean
  colors: {
    bar: number
    percentage: number
    phase: number
    details: number
    elapsed: number
  }
}

/**
 * Visual progress display for CLI operations
 */
export class ProgressDisplay {
  private config: ProgressConfig
  private currentState: ProgressState | null = null
  private isVisible = false
  private animationFrame: NodeJS.Timeout | null = null
  private animationIndex = 0

  private readonly spinnerFrames = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏']
  private readonly barFilled = '█'
  private readonly barEmpty = '░'

  constructor(config: Partial<ProgressConfig> = {}) {
    this.config = {
      barLength: 20,
      showPercentage: true,
      showPhase: true,
      showDetails: true,
      showElapsed: true,
      animated: true,
      colors: {
        bar: Colors.FG_GREEN,
        percentage: Colors.FG_CYAN,
        phase: Colors.FG_YELLOW,
        details: Colors.FG_WHITE,
        elapsed: Colors.FG_BLUE,
      },
      ...config,
    }
  }

  /**
   * Start showing progress
   */
  start(initialState?: Partial<ProgressState>): void {
    this.currentState = {
      phase: 'initializing',
      progress: 0,
      startTime: Date.now(),
      ...initialState,
    }

    this.isVisible = true
    this.render()

    if (this.config.animated) {
      this.startAnimation()
    }
  }

  /**
   * Update progress state
   */
  update(state: Partial<ProgressState>): void {
    if (!this.currentState) {
      this.start(state)
      return
    }

    this.currentState = {
      ...this.currentState,
      ...state,
    }

    this.render()
  }

  /**
   * Complete progress and hide
   */
  complete(message?: string): void {
    if (!this.isVisible) return

    // Show final state briefly
    this.update({
      progress: 1.0,
      phase: 'complete',
      message: message || 'Complete',
    })

    setTimeout(() => {
      this.hide()
    }, 1000)
  }

  /**
   * Hide progress display
   */
  hide(): void {
    if (!this.isVisible) return

    this.stopAnimation()

    // Clear the current line completely and position cursor at start
    Terminal.clearLine()
    Terminal.cursorToStart()
    process.stdout.write('\r') // Ensure carriage return

    this.isVisible = false
    this.currentState = null
  }

  /**
   * Hide and prepare for content replacement
   * This aggressively clears the screen area and positions for clean replacement
   */
  hideAndReplace(): void {
    if (!this.isVisible) return

    this.stopAnimation()

    // Clear current line and move up to clear any interfering content
    Terminal.clearLine()
    Terminal.cursorToStart()

    // Try to clear the previous few lines to remove debug output
    for (let i = 0; i < 3; i++) {
      process.stdout.write('\x1b[1A') // Move up 1 line
      Terminal.clearLine()
    }

    this.isVisible = false
    this.currentState = null
  }

  /**
   * Render the current progress state
   */
  private render(): void {
    if (!this.isVisible || !this.currentState) return

    const parts: string[] = []

    // Progress bar
    const bar = this.renderProgressBar(this.currentState.progress)
    parts.push(bar)

    // Percentage
    if (this.config.showPercentage) {
      Terminal.color(this.config.colors.percentage)
      const percentage = `${Math.round(this.currentState.progress * 100)}%`
      parts.push(percentage)
      Terminal.reset()
    }

    // Phase with spinner
    if (this.config.showPhase) {
      Terminal.color(this.config.colors.phase)
      const spinner = this.config.animated ? this.getCurrentSpinner() : ''
      const phase = `${spinner} ${this.currentState.phase}`
      parts.push(phase)
      Terminal.reset()
    }

    // Details
    if (this.config.showDetails && this.currentState.details) {
      Terminal.color(this.config.colors.details)
      parts.push(`- ${this.currentState.details}`)
      Terminal.reset()
    }

    // Elapsed time
    if (this.config.showElapsed && this.currentState.startTime) {
      Terminal.color(this.config.colors.elapsed)
      const elapsed = this.formatElapsed(Date.now() - this.currentState.startTime)
      parts.push(`(${elapsed})`)
      Terminal.reset()
    }

    // Clear line and render
    Terminal.clearLine()
    Terminal.cursorToStart()
    process.stdout.write(parts.join(' '))
  }

  /**
   * Render progress bar
   */
  private renderProgressBar(progress: number): string {
    const filled = Math.round(this.config.barLength * Math.min(1, Math.max(0, progress)))
    const empty = this.config.barLength - filled

    Terminal.color(this.config.colors.bar)
    const bar = `[${this.barFilled.repeat(filled)}${this.barEmpty.repeat(empty)}]`
    Terminal.reset()

    return bar
  }

  /**
   * Get current spinner frame
   */
  private getCurrentSpinner(): string {
    const frame = this.spinnerFrames[this.animationIndex % this.spinnerFrames.length]
    return frame || '⠋'
  }

  /**
   * Start spinner animation
   */
  private startAnimation(): void {
    if (this.animationFrame) {
      clearInterval(this.animationFrame)
    }

    this.animationFrame = setInterval(() => {
      this.animationIndex++
      this.render()
    }, 100) // Update every 100ms
  }

  /**
   * Stop spinner animation
   */
  private stopAnimation(): void {
    if (this.animationFrame) {
      clearInterval(this.animationFrame)
      this.animationFrame = null
    }
  }

  /**
   * Format elapsed time
   */
  private formatElapsed(ms: number): string {
    const seconds = Math.floor(ms / 1000)

    if (seconds < 60) {
      return `${seconds}s`
    }

    const minutes = Math.floor(seconds / 60)
    const remainingSeconds = seconds % 60

    return `${minutes}m ${remainingSeconds}s`
  }

  /**
   * Check if currently visible
   */
  isShowing(): boolean {
    return this.isVisible
  }

  /**
   * Get current state
   */
  getCurrentState(): ProgressState | null {
    return this.currentState ? { ...this.currentState } : null
  }

  /**
   * Update configuration
   */
  updateConfig(newConfig: Partial<ProgressConfig>): void {
    this.config = { ...this.config, ...newConfig }
  }

  /**
   * Clean up resources
   */
  destroy(): void {
    this.hide()
    this.stopAnimation()
  }
}

/**
 * Simple progress bar utility function
 */
export function createProgressBar(
  progress: number,
  length = 20,
  filled = '█',
  empty = '░'
): string {
  const filledLength = Math.round(length * Math.min(1, Math.max(0, progress)))
  const emptyLength = length - filledLength

  return `[${filled.repeat(filledLength)}${empty.repeat(emptyLength)}]`
}

/**
 * Format progress as percentage
 */
export function formatPercentage(progress: number): string {
  return `${Math.round(progress * 100)}%`
}

/**
 * Create a simple spinner
 */
export function createSpinner(frameIndex = 0): string {
  const frames = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏']
  const frame = frames[frameIndex % frames.length]
  return frame || '⠋'
}
