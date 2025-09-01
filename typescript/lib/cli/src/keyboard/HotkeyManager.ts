/**
 * Hotkey Manager for CLI - v-0.6.1 Pure Message-Driven
 *
 * Handles keyboard input detection and sends hotkey messages to message queue:
 * - Shift+Tab: Mode cycling
 * - Esc: Cancel current operation
 * - Ctrl+C: Graceful exit (SIGINT)
 *
 * COMPLETELY REDESIGNED: No events, no EventEmitter - pure message queue communication
 */

import type { QiAsyncMessageQueue } from '@qi/amsg'
import type { QiMessage } from '@qi/amsg'
import { MessagePriority, MessageType } from '@qi/amsg'
import { createDebugLogger } from '../utils/DebugLogger'

export interface HotkeyConfig {
  enableShiftTab: boolean
  enableEscape: boolean
  enableCtrlC: boolean
  passthrough: boolean // Whether to pass keys to readline
}

/**
 * Manages keyboard input and hotkey detection
 * v-0.6.1: Pure message-driven - sends hotkey messages to queue
 */
export class HotkeyManager {
  private isRawMode = false
  private config: HotkeyConfig
  private originalStdinListeners: Array<(...args: any[]) => void> = []
  private messageQueue: QiAsyncMessageQueue<QiMessage>
  private logger = createDebugLogger('HotkeyManager')

  constructor(messageQueue: QiAsyncMessageQueue<QiMessage>, config: Partial<HotkeyConfig> = {}) {
    // v-0.6.1: Pure message-driven constructor - requires message queue
    this.messageQueue = messageQueue

    this.config = {
      enableShiftTab: true,
      enableEscape: true,
      enableCtrlC: true,
      passthrough: true,
      ...config,
    }
  }

  /**
   * Enable raw mode and start listening for hotkeys
   */
  enable(): void {
    this.logger.trace('enable() called')
    if (this.isRawMode) {
      this.logger.trace('Already in raw mode, skipping')
      return // Already enabled
    }

    this.logger.trace('Enabling raw mode and setting up listeners')
    // Store existing listeners to restore later
    this.originalStdinListeners = process.stdin.listeners('data') as Array<(...args: any[]) => void>

    // Remove existing listeners to avoid conflicts
    process.stdin.removeAllListeners('data')

    // Enable raw mode for precise key detection
    if (process.stdin.setRawMode) {
      process.stdin.setRawMode(true)
    }

    process.stdin.resume()
    process.stdin.on('data', this.handleKeypress.bind(this))

    this.isRawMode = true
    // v-0.6.1: Event emission removed - pure message-driven
  }

  /**
   * Disable raw mode and restore normal input handling
   */
  disable(): void {
    if (!this.isRawMode) {
      return // Already disabled
    }

    // Remove our listener
    process.stdin.removeListener('data', this.handleKeypress.bind(this))

    // Disable raw mode
    if (process.stdin.setRawMode) {
      process.stdin.setRawMode(false)
    }

    // Restore original listeners
    for (const listener of this.originalStdinListeners) {
      process.stdin.on('data', listener)
    }

    this.isRawMode = false
    // v-0.6.1: Event emission removed - pure message-driven
  }

  /**
   * Handle individual keypress events
   */
  private handleKeypress(chunk: Buffer): void {
    const key = chunk.toString()
    const raw = chunk

    // Debug ALL keypresses
    const bytes = Array.from(chunk)
      .map((b) => `0x${b.toString(16).padStart(2, '0')}`)
      .join(' ')
    this.logger.trace(`Got keypress [${bytes}] "${key}"`)

    // v-0.6.1: Send hotkey messages to queue instead of events

    // Track if this key was handled as a global hotkey
    let wasGlobalHotkey = false

    // Check for hotkey combinations
    if (this.detectShiftTab(chunk)) {
      if (this.config.enableShiftTab) {
        // QiCore debug logging
        this.logger.trace('Processing Shift+Tab globally - blocking passthrough')
        this.sendHotkeyMessage('SHIFT_TAB', { key, raw })
        wasGlobalHotkey = true // Mark as handled globally
      }
    }

    if (this.detectEscape(chunk)) {
      if (this.config.enableEscape) {
        this.sendHotkeyMessage('ESCAPE', { key, raw })
        wasGlobalHotkey = true // Mark as handled globally
      }
    }

    if (this.detectCtrlC(chunk)) {
      if (this.config.enableCtrlC) {
        this.sendHotkeyMessage('CTRL_C', { key, raw })
        // Note: Ctrl+C should still be passed through for SIGINT even though it's global
      }
    }

    // CRITICAL FIX: Only pass through if NOT a global hotkey and passthrough is enabled
    // This prevents React components from seeing global hotkeys and causing race conditions
    if (this.config.passthrough && !wasGlobalHotkey) {
      this.passthroughKey(chunk)
    }
  }

  /**
   * Detect Shift+Tab key combination
   * Shift+Tab sends: ESC[Z (0x1b, 0x5b, 0x5a)
   */
  private detectShiftTab(chunk: Buffer): boolean {
    return chunk.length === 3 && chunk[0] === 0x1b && chunk[1] === 0x5b && chunk[2] === 0x5a
  }

  /**
   * Detect Escape key
   * Escape sends: ESC (0x1b) - but we need to distinguish from escape sequences
   */
  private detectEscape(chunk: Buffer): boolean {
    // Pure ESC key (not part of escape sequence)
    return chunk.length === 1 && chunk[0] === 0x1b
  }

  /**
   * Detect Ctrl+C key combination
   * Ctrl+C sends: ETX (0x03)
   */
  private detectCtrlC(chunk: Buffer): boolean {
    return chunk.length === 1 && chunk[0] === 0x03
  }

  /**
   * Pass key through to normal readline processing
   */
  private passthroughKey(chunk: Buffer): void {
    // Re-emit the data event for readline to process
    for (const listener of this.originalStdinListeners) {
      listener(chunk)
    }
  }

  /**
   * Check if hotkey detection is currently enabled
   */
  isEnabled(): boolean {
    return this.isRawMode
  }

  /**
   * Update configuration
   */
  updateConfig(newConfig: Partial<HotkeyConfig>): void {
    this.config = { ...this.config, ...newConfig }
    // v-0.6.1: Event emission removed - pure message-driven
  }

  /**
   * Send hotkey message to queue (v-0.6.1 pure message-driven approach)
   */
  private sendHotkeyMessage(hotkeyType: string, _data: { key: string; raw: Buffer }): void {
    const message: QiMessage = {
      id: Math.random().toString(36).substring(2, 15),
      type: MessageType.USER_INPUT, // Hotkeys are treated as special user input
      timestamp: new Date(),
      priority: MessagePriority.HIGH,
      input: `__HOTKEY_${hotkeyType}__`, // Special input format for hotkeys
      raw: true,
      source: 'cli' as const,
    }

    this.messageQueue.enqueue(message)
  }

  /**
   * Get current configuration
   */
  getConfig(): HotkeyConfig {
    return { ...this.config }
  }

  /**
   * Clean up resources
   */
  destroy(): void {
    this.disable()
    // v-0.6.1: No listeners to remove - pure message-driven
  }
}

/**
 * Utility function to create a HotkeyManager with common settings
 * v-0.6.1: Requires message queue parameter
 */
export function createHotkeyManager(
  messageQueue: QiAsyncMessageQueue<QiMessage>,
  config?: Partial<HotkeyConfig>
): HotkeyManager {
  return new HotkeyManager(messageQueue, config)
}

/**
 * Debug utility to display key codes
 */
export function debugKeypress(chunk: Buffer): string {
  const bytes = Array.from(chunk)
    .map((b) => `0x${b.toString(16).padStart(2, '0')}`)
    .join(' ')
  const chars = chunk
    .toString()
    .split('')
    .map((c) => {
      const code = c.charCodeAt(0)
      if (code < 32) {
        return `^${String.fromCharCode(code + 64)}`
      }
      return c
    })
    .join('')

  return `Bytes: [${bytes}] Chars: "${chars}"`
}
