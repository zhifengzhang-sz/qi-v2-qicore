/**
 * Input Manager abstraction interface
 *
 * Provides a framework-agnostic interface for input handling.
 * Implemented by ReadlineInputManager and framework adapters.
 */

export interface InputConfig {
  historySize?: number
  autoComplete?: boolean
  enableColors?: boolean
}

export interface KeypressData {
  name?: string
  sequence?: string
  ctrl?: boolean
  meta?: boolean
  shift?: boolean
}

/**
 * Core input handling interface
 * All input frameworks must implement this interface
 */
export interface IInputManager {
  /**
   * Initialize the input manager
   */
  initialize(config?: InputConfig): void

  /**
   * Set up input event handlers
   */
  onInput(callback: (input: string) => void): void

  /**
   * Set up keypress event handlers
   */
  onKeypress(callback: (key: string, data?: KeypressData) => void): void

  /**
   * Set up special key combination handlers
   */
  onShiftTab(callback: () => void): void
  onEscape(callback: () => void): void
  onCtrlC(callback: () => void): void
  onCtrlD(callback: () => void): void

  /**
   * Set the prompt string
   */
  setPrompt(prompt: string): void

  /**
   * Display the prompt
   */
  showPrompt(): void

  /**
   * Hide the prompt
   */
  hidePrompt(): void

  /**
   * Get current prompt string
   */
  getPrompt(): string

  /**
   * Clear current input line
   */
  clearInput(): void

  /**
   * Set current input text
   */
  setInput(text: string): void

  /**
   * Get current input text
   */
  getCurrentInput(): string

  /**
   * Add to input history
   */
  addToHistory(input: string): void

  /**
   * Get input history
   */
  getHistory(): string[]

  /**
   * Enable/disable input
   */
  setEnabled(enabled: boolean): void

  /**
   * Check if input is enabled
   */
  isEnabled(): boolean

  /**
   * Update configuration
   */
  updateConfig(config: Partial<InputConfig>): void

  /**
   * Clean up and close input manager
   */
  close(): void
}
