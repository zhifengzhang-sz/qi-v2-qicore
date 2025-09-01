/**
 * Readline Terminal implementation
 *
 * Implements ITerminal interface using Node.js built-ins and ANSI escape sequences.
 * This is the zero-dependency terminal implementation.
 */

import type { ITerminal, TerminalDimensions } from '../../abstractions/ITerminal'

/**
 * ANSI color codes for terminal formatting
 */
const ANSI = {
  // Cursor movement
  CLEAR_SCREEN: '\x1b[2J\x1b[H',
  CLEAR_LINE: '\r\x1b[K',
  CURSOR_TO_START: '\r',
  CURSOR_UP: (n: number) => `\x1b[${n}A`,
  CURSOR_DOWN: (n: number) => `\x1b[${n}B`,
  CURSOR_FORWARD: (n: number) => `\x1b[${n}C`,
  CURSOR_BACK: (n: number) => `\x1b[${n}D`,
  CURSOR_POSITION: (row: number, col: number) => `\x1b[${row};${col}H`,

  // Cursor visibility
  HIDE_CURSOR: '\x1b[?25l',
  SHOW_CURSOR: '\x1b[?25h',

  // Cursor save/restore
  SAVE_CURSOR: '\x1b[s',
  RESTORE_CURSOR: '\x1b[u',

  // Colors
  RESET: '\x1b[0m',
  BRIGHT: '\x1b[1m',
  DIM: '\x1b[2m',
  UNDERLINE: '\x1b[4m',

  // Foreground colors
  FG_BLACK: '\x1b[30m',
  FG_RED: '\x1b[31m',
  FG_GREEN: '\x1b[32m',
  FG_YELLOW: '\x1b[33m',
  FG_BLUE: '\x1b[34m',
  FG_MAGENTA: '\x1b[35m',
  FG_CYAN: '\x1b[36m',
  FG_WHITE: '\x1b[37m',

  // Background colors
  BG_BLACK: '\x1b[40m',
  BG_RED: '\x1b[41m',
  BG_GREEN: '\x1b[42m',
  BG_YELLOW: '\x1b[43m',
  BG_BLUE: '\x1b[44m',
  BG_MAGENTA: '\x1b[45m',
  BG_CYAN: '\x1b[46m',
  BG_WHITE: '\x1b[47m',
} as const

/**
 * Color code mapping
 */
const COLOR_MAP = {
  0: ANSI.RESET,
  1: ANSI.BRIGHT,
  2: ANSI.DIM,
  4: ANSI.UNDERLINE,

  // Foreground
  30: ANSI.FG_BLACK,
  31: ANSI.FG_RED,
  32: ANSI.FG_GREEN,
  33: ANSI.FG_YELLOW,
  34: ANSI.FG_BLUE,
  35: ANSI.FG_MAGENTA,
  36: ANSI.FG_CYAN,
  37: ANSI.FG_WHITE,

  // Background
  40: ANSI.BG_BLACK,
  41: ANSI.BG_RED,
  42: ANSI.BG_GREEN,
  43: ANSI.BG_YELLOW,
  44: ANSI.BG_BLUE,
  45: ANSI.BG_MAGENTA,
  46: ANSI.BG_CYAN,
  47: ANSI.BG_WHITE,
} as const

/**
 * Readline terminal implementation using Node.js built-ins
 * Zero external dependencies - uses only process.stdout and ANSI codes
 */
export class ReadlineTerminal implements ITerminal {
  private isDestroyed = false
  private colorSupported: boolean
  private unicodeSupported: boolean

  constructor() {
    // Detect terminal capabilities
    this.colorSupported = this.detectColorSupport()
    this.unicodeSupported = this.detectUnicodeSupport()
  }

  /**
   * Write text to the terminal
   */
  write(text: string): void {
    if (this.isDestroyed) {
      return
    }

    try {
      process.stdout.write(text)
    } catch (error) {
      console.error('ReadlineTerminal: Write failed:', error)
    }
  }

  /**
   * Clear the terminal screen
   */
  clear(): void {
    if (this.isDestroyed) {
      return
    }

    this.write(ANSI.CLEAR_SCREEN)
  }

  /**
   * Clear the current line
   */
  clearLine(): void {
    if (this.isDestroyed) {
      return
    }

    this.write(ANSI.CLEAR_LINE)
  }

  /**
   * Move cursor to beginning of current line
   */
  cursorToStart(): void {
    if (this.isDestroyed) {
      return
    }

    this.write(ANSI.CURSOR_TO_START)
  }

  /**
   * Move cursor up N lines
   */
  cursorUp(lines: number = 1): void {
    if (this.isDestroyed || lines <= 0) {
      return
    }

    this.write(ANSI.CURSOR_UP(lines))
  }

  /**
   * Move cursor down N lines
   */
  cursorDown(lines: number = 1): void {
    if (this.isDestroyed || lines <= 0) {
      return
    }

    this.write(ANSI.CURSOR_DOWN(lines))
  }

  /**
   * Move cursor to specific position (1-based coordinates)
   */
  moveCursor(x: number, y: number): void {
    if (this.isDestroyed) {
      return
    }

    // Ensure coordinates are at least 1
    const row = Math.max(1, Math.floor(y))
    const col = Math.max(1, Math.floor(x))

    this.write(ANSI.CURSOR_POSITION(row, col))
  }

  /**
   * Get terminal dimensions
   */
  getDimensions(): TerminalDimensions {
    return {
      width: process.stdout.columns || 80,
      height: process.stdout.rows || 24,
    }
  }

  /**
   * Save current cursor position
   */
  saveCursor(): void {
    if (this.isDestroyed) {
      return
    }

    this.write(ANSI.SAVE_CURSOR)
  }

  /**
   * Restore saved cursor position
   */
  restoreCursor(): void {
    if (this.isDestroyed) {
      return
    }

    this.write(ANSI.RESTORE_CURSOR)
  }

  /**
   * Hide cursor
   */
  hideCursor(): void {
    if (this.isDestroyed) {
      return
    }

    this.write(ANSI.HIDE_CURSOR)
  }

  /**
   * Show cursor
   */
  showCursor(): void {
    if (this.isDestroyed) {
      return
    }

    this.write(ANSI.SHOW_CURSOR)
  }

  /**
   * Set terminal colors/formatting
   */
  setColor(colorCode: number): void {
    if (this.isDestroyed || !this.colorSupported) {
      return
    }

    const ansiCode = COLOR_MAP[colorCode as keyof typeof COLOR_MAP]
    if (ansiCode) {
      this.write(ansiCode)
    } else {
      // Fallback for custom codes
      this.write(`\x1b[${colorCode}m`)
    }
  }

  /**
   * Reset all formatting
   */
  resetFormatting(): void {
    if (this.isDestroyed) {
      return
    }

    this.write(ANSI.RESET)
  }

  /**
   * Check if terminal supports colors
   */
  supportsColor(): boolean {
    return this.colorSupported
  }

  /**
   * Check if terminal supports Unicode
   */
  supportsUnicode(): boolean {
    return this.unicodeSupported
  }

  /**
   * Clean up resources
   */
  destroy(): void {
    if (this.isDestroyed) {
      return
    }

    // Reset terminal formatting
    this.resetFormatting()
    this.showCursor()

    this.isDestroyed = true
  }

  /**
   * Check if terminal has been destroyed
   */
  isTerminalDestroyed(): boolean {
    return this.isDestroyed
  }

  // Utility methods

  /**
   * Write formatted text with color support
   */
  writeColored(text: string, colorCode?: number): void {
    if (colorCode !== undefined && this.colorSupported) {
      this.setColor(colorCode)
      this.write(text)
      this.resetFormatting()
    } else {
      this.write(text)
    }
  }

  /**
   * Write a line with automatic newline
   */
  writeLine(text: string = ''): void {
    this.write(`${text}\n`)
  }

  /**
   * Write colored line
   */
  writeColoredLine(text: string, colorCode?: number): void {
    this.writeColored(`${text}\n`, colorCode)
  }

  /**
   * Clear line and write new content
   */
  replaceCurrentLine(text: string): void {
    this.clearLine()
    this.write(text)
  }

  /**
   * Bell/beep sound
   */
  bell(): void {
    if (this.isDestroyed) {
      return
    }

    this.write('\x07')
  }

  // Private methods

  private detectColorSupport(): boolean {
    // Check various environment indicators for color support
    const { env } = process

    // Explicit color support
    if (env.FORCE_COLOR && env.FORCE_COLOR !== '0') {
      return true
    }

    // Explicit no color
    if (env.NO_COLOR || env.NODE_DISABLE_COLORS) {
      return false
    }

    // Terminal type checks
    const term = env.TERM?.toLowerCase()
    if (term?.includes('color') || term?.includes('256') || term?.includes('truecolor')) {
      return true
    }

    // TTY check
    if (process.stdout.isTTY) {
      // Common terminals that support color
      const colorTerminals = [
        'xterm',
        'xterm-color',
        'xterm-256color',
        'screen',
        'screen-256color',
        'tmux',
        'tmux-256color',
        'rxvt',
        'ansi',
        'cygwin',
        'linux',
      ]

      return colorTerminals.some((t) => term?.includes(t))
    }

    return false
  }

  private detectUnicodeSupport(): boolean {
    const { env } = process

    // Check locale
    const locale = env.LC_ALL || env.LC_CTYPE || env.LANG || ''
    if (locale.toLowerCase().includes('utf')) {
      return true
    }

    // Check terminal
    const term = env.TERM?.toLowerCase()
    if (term?.includes('utf') || term?.includes('unicode')) {
      return true
    }

    // Default based on platform
    return process.platform !== 'win32'
  }
}
