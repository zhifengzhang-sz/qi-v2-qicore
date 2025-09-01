/**
 * CLI System - Main export file
 *
 * Event-driven CLI framework with hotkey support, progress display, and agent integration.
 */

// CLI Framework interfaces
// Re-export types for convenience
export type {
  CLIConfig,
  CLIConfig as Config,
  CLIEvents,
  CLIMode,
  CLIMode as Mode,
  CLIState,
  CLIState as State,
  IAgentCLIBridge,
  ICLIFramework,
  IKeyboardManager,
  IModeIndicator,
  IProgressDisplay,
  IStreamingRenderer,
  MessageType,
} from './abstractions/ICLIFramework'
// Legacy exports (maintained for backward compatibility)
export * from './abstractions/index'
export {
  autoDetectFramework,
  type CLIConfigWithFramework,
  type CLIFramework,
  displayConfigHelp,
  loadCLIConfig,
} from './config/index'
// Framework-agnostic factories and configuration
// ONLY ONE CLI CREATION FUNCTION - NO FALLBACKS
export {
  createCLIAsync,
  getAvailableFrameworks,
  recommendFramework,
} from './factories/createCLI'
export {
  createReadlineCLI,
  createReadlineCLIAsync,
  getDefaultReadlineConfig,
} from './factories/createReadlineCLI'
export * from './frameworks/index'
export * from './impl/index'
// NEW: Refactored CLI Framework with Dependency Injection
export { MessageDrivenCLI } from './impl/MessageDrivenCLI'
export type { HotkeyConfig } from './keyboard/HotkeyManager'
// Keyboard management
export { createHotkeyManager, debugKeypress, HotkeyManager } from './keyboard/HotkeyManager'
export {
  Colors,
  controlToString,
  EscapeSequences,
  identifyKey,
  isControlCharacter,
  isPrintable,
  KeyCodes,
  matchesEscapeSequence,
  Terminal,
} from './keyboard/KeyboardUtils'
export type { ModeConfig, ModeInfo } from './ui/ModeIndicator'
export {
  createModeIndicator,
  getModeColor,
  getModeEmoji,
  ModeIndicator,
} from './ui/ModeIndicator'
export type { ProgressConfig, ProgressState } from './ui/ProgressDisplay'
// UI Components
export {
  createProgressBar,
  createSpinner,
  formatPercentage,
  ProgressDisplay,
} from './ui/ProgressDisplay'
export type { StreamChunk } from './ui/StreamingRenderer'
export { createStreamingRenderer, StreamingRenderer, wrapText } from './ui/StreamingRenderer'

// Default configuration
export const DefaultCLIConfig: import('./abstractions/ICLIFramework').CLIConfig = {
  enableHotkeys: true,
  enableModeIndicator: true,
  enableProgressDisplay: true,
  enableStreaming: true,
  prompt: '> ',
  colors: true,
  animations: true,
  historySize: 100,
  autoComplete: false,
  streamingThrottle: 0,
  maxBufferSize: 10000,
  debug: false,
}
