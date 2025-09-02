/**
 * Mode Indicator Component
 *
 * Shows the current CLI mode and provides visual feedback for mode switching:
 * - Interactive mode: Normal conversational interaction
 * - Command mode: Command-focused with enhanced command completion
 * - Streaming mode: Real-time streaming responses
 */

import { Colors, Terminal } from '../keyboard/KeyboardUtils'

export type CLIMode = 'interactive' | 'command' | 'streaming'

export interface ModeConfig {
  showIcon: boolean
  showLabel: boolean
  showHotkey: boolean
  position: 'left' | 'right'
  colors: {
    [K in CLIMode]: {
      background: number
      foreground: number
      icon: string
      label: string
    }
  }
}

export interface ModeInfo {
  mode: CLIMode
  description: string
  capabilities: string[]
  hotkey: string
}

/**
 * Mode information for each CLI mode
 */
const ModeDefinitions: Record<CLIMode, ModeInfo> = {
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
 * Visual mode indicator for CLI
 */
export class ModeIndicator {
  private currentMode: CLIMode = 'interactive'
  private config: ModeConfig
  private isVisible = false

  constructor(config: Partial<ModeConfig> = {}) {
    this.config = {
      showIcon: true,
      showLabel: true,
      showHotkey: false,
      position: 'left',
      colors: {
        interactive: {
          background: Colors.BG_BLUE,
          foreground: Colors.FG_WHITE,
          icon: '💬',
          label: 'Interactive',
        },
        command: {
          background: Colors.BG_YELLOW,
          foreground: Colors.FG_BLACK,
          icon: '⚡',
          label: 'Command',
        },
        streaming: {
          background: Colors.BG_GREEN,
          foreground: Colors.FG_WHITE,
          icon: '🌊',
          label: 'Streaming',
        },
      },
      ...config,
    }
  }

  /**
   * Set the current mode
   */
  setMode(mode: CLIMode, silent = false): void {
    const previousMode = this.currentMode
    this.currentMode = mode

    if (this.isVisible) {
      this.render()

      // Show brief transition message only if not silent
      if (previousMode !== mode && !silent) {
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
  cycleMode(silent = false): CLIMode {
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
   * Show the mode indicator
   */
  show(): void {
    this.isVisible = true
    this.render()
  }

  /**
   * Hide the mode indicator
   */
  hide(): void {
    if (!this.isVisible) return

    this.isVisible = false
    // Clear the mode indicator area if needed
  }

  /**
   * Render the mode indicator
   */
  private render(): void {
    if (!this.isVisible) return

    const modeInfo = ModeDefinitions[this.currentMode]
    const colorConfig = this.config.colors[this.currentMode]
    const parts: string[] = []

    // Set background and foreground colors
    Terminal.color(colorConfig.background)
    Terminal.color(colorConfig.foreground)

    // Add icon
    if (this.config.showIcon) {
      parts.push(colorConfig.icon)
    }

    // Add label
    if (this.config.showLabel) {
      parts.push(colorConfig.label)
    }

    // Add hotkey hint
    if (this.config.showHotkey) {
      parts.push(`(${modeInfo.hotkey})`)
    }

    const indicator = parts.length > 0 ? ` ${parts.join(' ')} ` : ''

    // Render based on position
    if (this.config.position === 'left') {
      this.renderLeft(indicator)
    } else {
      this.renderRight(indicator)
    }

    Terminal.reset()
  }

  /**
   * Render mode indicator on the left
   */
  private renderLeft(indicator: string): void {
    Terminal.saveCursor()
    Terminal.cursorToStart()
    process.stdout.write(indicator)
    Terminal.restoreCursor()
  }

  /**
   * Render mode indicator on the right
   */
  private renderRight(indicator: string): void {
    const terminalWidth = Terminal.getDimensions().width
    const indicatorWidth = indicator.length
    const position = Math.max(0, terminalWidth - indicatorWidth)

    Terminal.saveCursor()
    Terminal.cursorToStart()
    process.stdout.write(' '.repeat(position) + indicator)
    Terminal.restoreCursor()
  }

  /**
   * Show mode transition message
   */
  private showTransition(fromMode: CLIMode, toMode: CLIMode): void {
    const _fromInfo = ModeDefinitions[fromMode]
    const _toInfo = ModeDefinitions[toMode]
    const toColorConfig = this.config.colors[toMode]

    // Brief notification
    Terminal.color(toColorConfig.background)
    Terminal.color(toColorConfig.foreground)

    const message = `${toColorConfig.icon} Switched to ${toColorConfig.label} mode`
    console.log(`\n${message}`)

    Terminal.reset()

    // Clear after a short delay
    setTimeout(() => {
      Terminal.cursorUp(1)
      Terminal.clearLine()
    }, 1500)
  }

  /**
   * Get mode information
   */
  getModeInfo(mode?: CLIMode): ModeInfo {
    return ModeDefinitions[mode || this.currentMode]
  }

  /**
   * Get all available modes
   */
  getAllModes(): ModeInfo[] {
    return Object.values(ModeDefinitions)
  }

  /**
   * Show mode help
   */
  showModeHelp(): void {
    const modeInfo = this.getModeInfo()
    const colorConfig = this.config.colors[this.currentMode]

    console.log(`\n${colorConfig.icon} ${colorConfig.label} Mode`)
    console.log(`${modeInfo.description}\n`)

    console.log('Capabilities:')
    for (const capability of modeInfo.capabilities) {
      console.log(`  • ${capability}`)
    }

    console.log(`\nPress ${modeInfo.hotkey} to cycle modes\n`)
  }

  /**
   * Get formatted prompt prefix
   */
  getPromptPrefix(): string {
    const colorConfig = this.config.colors[this.currentMode]

    let prefix = ''
    if (this.config.showIcon) {
      prefix += `${colorConfig.icon} `
    }

    return prefix
  }

  /**
   * Get current configuration
   */
  getConfig(): ModeConfig {
    return { ...this.config }
  }

  /**
   * Update configuration
   */
  updateConfig(newConfig: Partial<ModeConfig>): void {
    this.config = { ...this.config, ...newConfig }

    if (this.isVisible) {
      this.render()
    }
  }

  /**
   * Check if mode indicator is visible
   */
  isShowing(): boolean {
    return this.isVisible
  }

  /**
   * Clean up resources
   */
  destroy(): void {
    this.hide()
  }
}

/**
 * Utility function to create a mode indicator
 */
export function createModeIndicator(config?: Partial<ModeConfig>): ModeIndicator {
  return new ModeIndicator(config)
}

/**
 * Utility function to get mode emoji
 */
export function getModeEmoji(mode: CLIMode): string {
  const _modeInfo = ModeDefinitions[mode]
  return mode === 'interactive' ? '💬' : mode === 'command' ? '⚡' : '🌊'
}

/**
 * Utility function to get mode color
 */
export function getModeColor(mode: CLIMode): { bg: number; fg: number } {
  return mode === 'interactive'
    ? { bg: Colors.BG_BLUE, fg: Colors.FG_WHITE }
    : mode === 'command'
      ? { bg: Colors.BG_YELLOW, fg: Colors.FG_BLACK }
      : { bg: Colors.BG_GREEN, fg: Colors.FG_WHITE }
}
