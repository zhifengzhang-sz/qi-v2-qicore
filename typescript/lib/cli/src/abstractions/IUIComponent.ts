/**
 * UI Component abstraction interfaces
 *
 * Provides framework-agnostic interfaces for UI components.
 * Implemented differently by each terminal framework.
 */

import type { CLIMode } from '../abstractions/ICLIFramework'

/**
 * Base UI component interface
 */
export interface IUIComponent {
  /**
   * Show the component
   */
  show(): void

  /**
   * Hide the component
   */
  hide(): void

  /**
   * Check if component is visible
   */
  isVisible(): boolean

  /**
   * Update component configuration
   */
  updateConfig(config: any): void

  /**
   * Clean up component resources
   */
  destroy(): void
}

/**
 * Progress display configuration
 */
export interface ProgressConfig {
  phase?: string
  progress?: number
  details?: string
  animated?: boolean
  showPercentage?: boolean
  showElapsed?: boolean
}

/**
 * Progress renderer interface
 */
export interface IProgressRenderer extends IUIComponent {
  /**
   * Start showing progress
   */
  start(config?: ProgressConfig): void

  /**
   * Update progress state
   */
  updateProgress(progress: number, phase?: string, details?: string): void

  /**
   * Complete progress and optionally show final message
   */
  complete(message?: string): void

  /**
   * Hide progress and prepare for content replacement
   */
  hideAndReplace(): void

  /**
   * Get current progress state
   */
  getCurrentState(): ProgressConfig | null

  /**
   * Check if progress is currently showing
   */
  isShowing(): boolean
}

/**
 * Mode renderer interface
 */
export interface IModeRenderer extends IUIComponent {
  /**
   * Set the current mode
   */
  setMode(mode: CLIMode, silent?: boolean): void

  /**
   * Get the current mode
   */
  getMode(): CLIMode

  /**
   * Cycle to the next mode
   */
  cycleMode(silent?: boolean): CLIMode

  /**
   * Get prompt prefix for current mode
   */
  getPromptPrefix(): string

  /**
   * Show mode help information
   */
  showModeHelp(): void

  /**
   * Get mode information
   */
  getModeInfo(mode?: CLIMode): any
}

/**
 * Stream renderer interface
 */
export interface IStreamRenderer extends IUIComponent {
  /**
   * Start streaming output
   */
  startStreaming(): void

  /**
   * Add a chunk of content to the stream
   */
  addChunk(content: string): void

  /**
   * Complete the streaming process
   */
  complete(message?: string): void

  /**
   * Cancel streaming
   */
  cancel(): void

  /**
   * Check if currently streaming
   */
  isStreaming(): boolean

  /**
   * Get current streaming state
   */
  getStreamingState(): any

  /**
   * Set up streaming event handlers
   */
  onStreamingComplete(callback: (content: string) => void): void
  onStreamingCancelled(callback: () => void): void
}

/**
 * Generic message display interface
 */
export interface IMessageDisplay extends IUIComponent {
  /**
   * Display a message
   */
  displayMessage(content: string, type?: string): void

  /**
   * Clear all messages
   */
  clearMessages(): void

  /**
   * Get message history
   */
  getMessageHistory(): Array<{ content: string; type: string; timestamp: Date }>
}
