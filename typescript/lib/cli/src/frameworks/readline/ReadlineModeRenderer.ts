/**
 * Readline Mode Renderer implementation
 *
 * Implements IModeRenderer interface using text-based mode indicators
 * and ANSI escape sequences for colors.
 */

import type { CLIMode } from '../../abstractions/ICLIFramework'
import type { IModeRenderer } from '../../abstractions/IUIComponent'

/**
 * Mode display configuration
 */
interface ModeDisplayConfig {
  showIcon: boolean
  showLabel: boolean
  showHotkey: boolean
  position: 'left' | 'right' | 'inline'
  colors: {
    [K in CLIMode]: {
      background: number
      foreground: number
      icon: string
      label: string
    }
  }
}

/**
 * Mode information interface
 */
interface ModeInfo {
  mode: CLIMode
  description: string
  capabilities: string[]
  hotkey: string
}

/**
 * Mode definitions with metadata
 */
const MODE_DEFINITIONS: Record<CLIMode, ModeInfo> = {
  interactive: {
    mode: 'interactive',
    description: 'Interactive conversation mode',
    capabilities: [
      'Natural language prompts',
      'Context-aware responses',
      'Conversation history',
      'Basic commands',
    ],
    hotkey: 'Shift+Tab',
  },

  command: {
    mode: 'command',
    description: 'Command-focused mode',
    capabilities: [
      'Enhanced command completion',
      'Command history',
      'Built-in help system',
      'Faster command execution',
    ],
    hotkey: 'Shift+Tab',
  },

  streaming: {
    mode: 'streaming',
    description: 'Real-time streaming mode',
    capabilities: [
      'Character-by-character responses',
      'Live progress indicators',
      'Real-time cancellation',
      'Streaming telemetry',
    ],
    hotkey: 'Shift+Tab',
  },
}

/**
 * Default configuration
 */
const DEFAULT_CONFIG: ModeDisplayConfig = {
  showIcon: true,
  showLabel: true,
  showHotkey: false,
  position: 'inline',
  colors: {
    interactive: {
      background: 44, // Blue background
      foreground: 37, // White foreground
      icon: '💬',
      label: 'Interactive',
    },
    command: {
      background: 43, // Yellow background
      foreground: 30, // Black foreground
      icon: '⚡',
      label: 'Command',
    },
    streaming: {
      background: 42, // Green background
      foreground: 37, // White foreground
      icon: '🌊',
      label: 'Streaming',
    },
  },
}

/**
 * Readline mode renderer using text indicators and ANSI colors
 * Provides visual feedback for current CLI mode
 */
export class ReadlineModeRenderer implements IModeRenderer {
  private config: ModeDisplayConfig
  private currentMode: CLIMode = 'interactive'
  private visible = false
  private isDestroyed = false

  constructor(config: Partial<ModeDisplayConfig> = {}) {
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
    this.render()
  }

  /**
   * Hide the component
   */
  hide(): void {
    if (this.isDestroyed) {
      return
    }

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
  updateConfig(config: Partial<ModeDisplayConfig>): void {
    if (this.isDestroyed) {
      return
    }

    this.config = { ...this.config, ...config }

    if (this.visible) {
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
   * Set the current mode
   */
  setMode(mode: CLIMode, silent: boolean = false): void {
    if (this.isDestroyed) {
      return
    }

    const previousMode = this.currentMode
    this.currentMode = mode

    if (this.visible) {
      this.render()

      // Show transition message if not silent and mode changed
      if (!silent && previousMode !== mode) {
        this.showTransition(previousMode, mode)
      }
    }
  }

  /**
   * Get the current mode
   */
  getMode(): CLIMode {
    return this.currentMode
  }

  /**
   * Cycle to the next mode
   */
  cycleMode(silent: boolean = false): CLIMode {
    const modes: CLIMode[] = ['interactive', 'command', 'streaming']
    const currentIndex = modes.indexOf(this.currentMode)
    const nextIndex = (currentIndex + 1) % modes.length
    const nextMode = modes[nextIndex]

    if (nextMode) {
      this.setMode(nextMode, silent)
      return nextMode
    }

    // Fallback to current mode if array access fails
    return this.currentMode
  }

  /**
   * Get prompt prefix for current mode
   */
  getPromptPrefix(): string {
    if (this.isDestroyed || this.config.position !== 'inline') {
      return ''
    }

    const modeConfig = this.config.colors[this.currentMode]
    const parts: string[] = []

    if (this.config.showIcon) {
      parts.push(modeConfig.icon)
    }

    if (this.config.showLabel) {
      parts.push(modeConfig.label)
    }

    if (parts.length === 0) {
      return ''
    }

    const content = parts.join(' ')

    if (this.supportsColor()) {
      return `${this.colorize(` ${content} `, modeConfig.background, modeConfig.foreground)} `
    } else {
      return `[${content}] `
    }
  }

  /**
   * Show mode help information
   */
  showModeHelp(): void {
    if (this.isDestroyed) {
      return
    }

    const modeInfo = this.getModeInfo()
    const modeConfig = this.config.colors[this.currentMode]

    console.log(`\n${modeConfig.icon} ${modeConfig.label} Mode`)
    console.log(`${modeInfo.description}\n`)

    console.log('Capabilities:')
    for (const capability of modeInfo.capabilities) {
      console.log(`  • ${capability}`)
    }

    console.log(`\nPress ${modeInfo.hotkey} to cycle modes\n`)
  }

  /**
   * Get mode information
   */
  getModeInfo(mode?: CLIMode): ModeInfo {
    const targetMode = mode || this.currentMode
    return MODE_DEFINITIONS[targetMode]
  }

  /**
   * Get all available modes
   */
  getAllModes(): ModeInfo[] {
    return Object.values(MODE_DEFINITIONS)
  }

  /**
   * Get formatted mode status
   */
  getModeStatus(): string {
    const modeInfo = this.getModeInfo()
    const modeConfig = this.config.colors[this.currentMode]

    return `Current mode: ${modeConfig.icon} ${modeConfig.label} - ${modeInfo.description}`
  }

  // Private methods

  private render(): void {
    if (!this.visible || this.isDestroyed || this.config.position === 'inline') {
      return
    }

    const indicator = this.buildModeIndicator()

    if (this.config.position === 'left') {
      this.renderLeft(indicator)
    } else if (this.config.position === 'right') {
      this.renderRight(indicator)
    }
  }

  private buildModeIndicator(): string {
    const modeConfig = this.config.colors[this.currentMode]
    const modeInfo = MODE_DEFINITIONS[this.currentMode]
    const parts: string[] = []

    if (this.config.showIcon) {
      parts.push(modeConfig.icon)
    }

    if (this.config.showLabel) {
      parts.push(modeConfig.label)
    }

    if (this.config.showHotkey) {
      parts.push(`(${modeInfo.hotkey})`)
    }

    const content = parts.join(' ')

    if (this.supportsColor()) {
      return this.colorize(` ${content} `, modeConfig.background, modeConfig.foreground)
    } else {
      return `[${content}]`
    }
  }

  private renderLeft(indicator: string): void {
    // Save cursor position
    process.stdout.write('\x1b[s')

    // Move to start of line
    process.stdout.write('\r')

    // Write indicator
    process.stdout.write(indicator)

    // Restore cursor position
    process.stdout.write('\x1b[u')
  }

  private renderRight(indicator: string): void {
    const terminalWidth = this.getTerminalWidth()
    const indicatorWidth = this.getDisplayWidth(indicator)
    const position = Math.max(0, terminalWidth - indicatorWidth)

    // Save cursor position
    process.stdout.write('\x1b[s')

    // Move to calculated position
    process.stdout.write('\r')
    process.stdout.write(' '.repeat(position))
    process.stdout.write(indicator)

    // Restore cursor position
    process.stdout.write('\x1b[u')
  }

  private showTransition(fromMode: CLIMode, toMode: CLIMode): void {
    const _fromInfo = MODE_DEFINITIONS[fromMode]
    const _toInfo = MODE_DEFINITIONS[toMode]
    const toConfig = this.config.colors[toMode]

    // Brief notification
    let message = `${toConfig.icon} Switched to ${toConfig.label} mode`

    if (this.supportsColor()) {
      message = this.colorize(message, toConfig.background, toConfig.foreground)
    }

    console.log(`\n${message}`)

    // Clear message after delay
    setTimeout(() => {
      process.stdout.write('\x1b[1A') // Move up 1 line
      process.stdout.write('\r\x1b[K') // Clear line
    }, 1500)
  }

  private colorize(text: string, bgColor: number, fgColor: number): string {
    return `\x1b[${bgColor}m\x1b[${fgColor}m${text}\x1b[0m`
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

  private getTerminalWidth(): number {
    return process.stdout.columns || 80
  }

  private getDisplayWidth(text: string): number {
    // Remove ANSI escape sequences for width calculation
    // biome-ignore lint/suspicious/noControlCharactersInRegex: ANSI escape sequence is intentional
    const cleaned = text.replace(/\x1b\[[0-9;]*m/g, '')

    // Count visual characters (Unicode aware)
    let width = 0
    for (const char of cleaned) {
      // Emoji and wide characters typically take 2 spaces
      const code = char.codePointAt(0) || 0
      if (
        code > 0x1f600 ||
        (code >= 0x1100 &&
          (code <= 0x115f || // Hangul Jamo
            code === 0x2329 ||
            code === 0x232a ||
            (code >= 0x2e80 && code <= 0x3247) ||
            (code >= 0x3250 && code <= 0x4dbf) ||
            (code >= 0x4e00 && code <= 0xa4cf) ||
            (code >= 0xa960 && code <= 0xa97f) ||
            (code >= 0xac00 && code <= 0xd7a3) ||
            (code >= 0xf900 && code <= 0xfaff) ||
            (code >= 0xfe10 && code <= 0xfe19) ||
            (code >= 0xfe30 && code <= 0xfe6f) ||
            (code >= 0xff00 && code <= 0xff60) ||
            (code >= 0xffe0 && code <= 0xffe6) ||
            (code >= 0x20000 && code <= 0x2fffd) ||
            (code >= 0x30000 && code <= 0x3fffd)))
      ) {
        width += 2
      } else {
        width += 1
      }
    }

    return width
  }

  /**
   * Get mode statistics
   */
  getStats(): {
    currentMode: CLIMode
    isVisible: boolean
    supportsColor: boolean
    terminalWidth: number
  } {
    return {
      currentMode: this.currentMode,
      isVisible: this.visible,
      supportsColor: this.supportsColor(),
      terminalWidth: this.getTerminalWidth(),
    }
  }
}
