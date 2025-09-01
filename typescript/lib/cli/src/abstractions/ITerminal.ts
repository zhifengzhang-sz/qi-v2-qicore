/**
 * Terminal abstraction interface
 *
 * Provides a framework-agnostic interface for terminal operations.
 * Implemented by ReadlineTerminal (Ink/Hybrid use React components instead).
 */

export interface TerminalDimensions {
  width: number
  height: number
}

/**
 * Core terminal operations interface
 * All terminal frameworks must implement this interface
 */
export interface ITerminal {
  /**
   * Write text to the terminal
   */
  write(text: string): void

  /**
   * Write text with automatic newline
   */
  writeLine(text?: string): void

  /**
   * Clear the terminal screen
   */
  clear(): void

  /**
   * Clear the current line
   */
  clearLine(): void

  /**
   * Move cursor to beginning of current line
   */
  cursorToStart(): void

  /**
   * Move cursor up N lines
   */
  cursorUp(lines?: number): void

  /**
   * Move cursor down N lines
   */
  cursorDown(lines?: number): void

  /**
   * Move cursor to specific position
   */
  moveCursor(x: number, y: number): void

  /**
   * Get terminal dimensions
   */
  getDimensions(): TerminalDimensions

  /**
   * Save current cursor position
   */
  saveCursor(): void

  /**
   * Restore saved cursor position
   */
  restoreCursor(): void

  /**
   * Hide cursor
   */
  hideCursor(): void

  /**
   * Show cursor
   */
  showCursor(): void

  /**
   * Set terminal colors/formatting
   */
  setColor(colorCode: number): void

  /**
   * Reset all formatting
   */
  resetFormatting(): void

  /**
   * Check if terminal supports colors
   */
  supportsColor(): boolean

  /**
   * Check if terminal supports Unicode
   */
  supportsUnicode(): boolean

  /**
   * Clean up resources
   */
  destroy(): void
}
