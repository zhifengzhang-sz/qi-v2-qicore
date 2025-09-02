/**
 * Keyboard Utilities
 *
 * Helper functions for keyboard input handling, key detection, and terminal control.
 */

/**
 * Common key codes and escape sequences
 */
export const KeyCodes = {
  // Control characters
  NULL: 0x00,
  CTRL_A: 0x01,
  CTRL_B: 0x02,
  CTRL_C: 0x03,
  CTRL_D: 0x04,
  CTRL_E: 0x05,
  CTRL_F: 0x06,
  CTRL_G: 0x07,
  BACKSPACE: 0x08,
  TAB: 0x09,
  CTRL_J: 0x0a,
  CTRL_K: 0x0b,
  CTRL_L: 0x0c,
  ENTER: 0x0d,
  CTRL_N: 0x0e,
  CTRL_O: 0x0f,
  CTRL_P: 0x10,
  CTRL_Q: 0x11,
  CTRL_R: 0x12,
  CTRL_S: 0x13,
  CTRL_T: 0x14,
  CTRL_U: 0x15,
  CTRL_V: 0x16,
  CTRL_W: 0x17,
  CTRL_X: 0x18,
  CTRL_Y: 0x19,
  CTRL_Z: 0x1a,
  ESCAPE: 0x1b,
  DELETE: 0x7f,

  // Printable range
  SPACE: 0x20,
  TILDE: 0x7e,
} as const

/**
 * Escape sequences for special keys
 */
export const EscapeSequences = {
  // Arrow keys
  UP: Buffer.from([0x1b, 0x5b, 0x41]),
  DOWN: Buffer.from([0x1b, 0x5b, 0x42]),
  RIGHT: Buffer.from([0x1b, 0x5b, 0x43]),
  LEFT: Buffer.from([0x1b, 0x5b, 0x44]),

  // Function keys (F1-F12)
  F1: Buffer.from([0x1b, 0x4f, 0x50]),
  F2: Buffer.from([0x1b, 0x4f, 0x51]),
  F3: Buffer.from([0x1b, 0x4f, 0x52]),
  F4: Buffer.from([0x1b, 0x4f, 0x53]),

  // Special combinations
  SHIFT_TAB: Buffer.from([0x1b, 0x5b, 0x5a]),

  // Home/End/Page keys
  HOME: Buffer.from([0x1b, 0x5b, 0x48]),
  END: Buffer.from([0x1b, 0x5b, 0x46]),
  PAGE_UP: Buffer.from([0x1b, 0x5b, 0x35, 0x7e]),
  PAGE_DOWN: Buffer.from([0x1b, 0x5b, 0x36, 0x7e]),

  // Insert/Delete
  INSERT: Buffer.from([0x1b, 0x5b, 0x32, 0x7e]),
  DELETE_KEY: Buffer.from([0x1b, 0x5b, 0x33, 0x7e]),
} as const

/**
 * Check if a buffer matches a specific escape sequence
 */
export function matchesEscapeSequence(chunk: Buffer, sequence: Buffer): boolean {
  if (chunk.length !== sequence.length) {
    return false
  }

  for (let i = 0; i < chunk.length; i++) {
    if (chunk[i] !== sequence[i]) {
      return false
    }
  }

  return true
}

/**
 * Check if a key is a control character
 */
export function isControlCharacter(byte: number): boolean {
  return byte >= 0x00 && byte <= 0x1f
}

/**
 * Check if a key is printable
 */
export function isPrintable(byte: number): boolean {
  return byte >= KeyCodes.SPACE && byte <= KeyCodes.TILDE
}

/**
 * Convert control character to human-readable string
 */
export function controlToString(byte: number): string {
  if (!isControlCharacter(byte)) {
    return String.fromCharCode(byte)
  }

  const controlNames: Record<number, string> = {
    [KeyCodes.NULL]: 'NULL',
    [KeyCodes.CTRL_A]: 'CTRL+A',
    [KeyCodes.CTRL_B]: 'CTRL+B',
    [KeyCodes.CTRL_C]: 'CTRL+C',
    [KeyCodes.CTRL_D]: 'CTRL+D',
    [KeyCodes.CTRL_E]: 'CTRL+E',
    [KeyCodes.CTRL_F]: 'CTRL+F',
    [KeyCodes.CTRL_G]: 'CTRL+G',
    [KeyCodes.BACKSPACE]: 'BACKSPACE',
    [KeyCodes.TAB]: 'TAB',
    [KeyCodes.CTRL_J]: 'CTRL+J',
    [KeyCodes.CTRL_K]: 'CTRL+K',
    [KeyCodes.CTRL_L]: 'CTRL+L',
    [KeyCodes.ENTER]: 'ENTER',
    [KeyCodes.CTRL_N]: 'CTRL+N',
    [KeyCodes.CTRL_O]: 'CTRL+O',
    [KeyCodes.CTRL_P]: 'CTRL+P',
    [KeyCodes.CTRL_Q]: 'CTRL+Q',
    [KeyCodes.CTRL_R]: 'CTRL+R',
    [KeyCodes.CTRL_S]: 'CTRL+S',
    [KeyCodes.CTRL_T]: 'CTRL+T',
    [KeyCodes.CTRL_U]: 'CTRL+U',
    [KeyCodes.CTRL_V]: 'CTRL+V',
    [KeyCodes.CTRL_W]: 'CTRL+W',
    [KeyCodes.CTRL_X]: 'CTRL+X',
    [KeyCodes.CTRL_Y]: 'CTRL+Y',
    [KeyCodes.CTRL_Z]: 'CTRL+Z',
    [KeyCodes.ESCAPE]: 'ESC',
    [KeyCodes.DELETE]: 'DELETE',
  }

  return controlNames[byte] || `CTRL+${String.fromCharCode(byte + 64)}`
}

/**
 * Identify special key combinations
 */
export function identifyKey(chunk: Buffer): string {
  // Single byte keys
  if (chunk.length === 1) {
    const byte = chunk[0]

    if (byte === undefined) {
      return 'UNKNOWN'
    }

    if (isControlCharacter(byte)) {
      return controlToString(byte)
    }

    if (isPrintable(byte)) {
      return String.fromCharCode(byte)
    }

    return `UNKNOWN(0x${byte.toString(16)})`
  }

  // Multi-byte escape sequences
  for (const [name, sequence] of Object.entries(EscapeSequences)) {
    if (matchesEscapeSequence(chunk, sequence)) {
      return name
    }
  }

  // Unknown sequence
  const bytes = Array.from(chunk)
    .map((b) => `0x${b.toString(16).padStart(2, '0')}`)
    .join(' ')
  return `UNKNOWN_SEQ(${bytes})`
}

/**
 * Terminal control functions
 */
export const Terminal = {
  /**
   * Clear the current line
   */
  clearLine(): void {
    process.stdout.write('\r\x1b[K')
  },

  /**
   * Move cursor to beginning of line
   */
  cursorToStart(): void {
    process.stdout.write('\r')
  },

  /**
   * Move cursor up N lines
   */
  cursorUp(lines = 1): void {
    if (lines > 0) {
      process.stdout.write(`\x1b[${lines}A`)
    }
  },

  /**
   * Move cursor down N lines
   */
  cursorDown(lines = 1): void {
    if (lines > 0) {
      process.stdout.write(`\x1b[${lines}B`)
    }
  },

  /**
   * Hide cursor
   */
  hideCursor(): void {
    process.stdout.write('\x1b[?25l')
  },

  /**
   * Show cursor
   */
  showCursor(): void {
    process.stdout.write('\x1b[?25h')
  },

  /**
   * Save cursor position
   */
  saveCursor(): void {
    process.stdout.write('\x1b[s')
  },

  /**
   * Restore cursor position
   */
  restoreCursor(): void {
    process.stdout.write('\x1b[u')
  },

  /**
   * Set text color
   */
  color(colorCode: number): void {
    process.stdout.write(`\x1b[${colorCode}m`)
  },

  /**
   * Reset all formatting
   */
  reset(): void {
    process.stdout.write('\x1b[0m')
  },

  /**
   * Get terminal dimensions
   */
  getDimensions(): { width: number; height: number } {
    return {
      width: process.stdout.columns || 80,
      height: process.stdout.rows || 24,
    }
  },
}

/**
 * Common terminal colors
 */
export const Colors = {
  RESET: 0,
  BRIGHT: 1,
  DIM: 2,
  UNDERLINE: 4,
  BLINK: 5,
  REVERSE: 7,
  HIDDEN: 8,

  // Foreground colors
  FG_BLACK: 30,
  FG_RED: 31,
  FG_GREEN: 32,
  FG_YELLOW: 33,
  FG_BLUE: 34,
  FG_MAGENTA: 35,
  FG_CYAN: 36,
  FG_WHITE: 37,

  // Background colors
  BG_BLACK: 40,
  BG_RED: 41,
  BG_GREEN: 42,
  BG_YELLOW: 43,
  BG_BLUE: 44,
  BG_MAGENTA: 45,
  BG_CYAN: 46,
  BG_WHITE: 47,
} as const
