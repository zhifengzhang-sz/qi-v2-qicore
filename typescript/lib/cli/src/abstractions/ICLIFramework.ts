/**
 * CLI Framework Interfaces
 *
 * Defines the contracts for event-driven CLI components
 */

export interface CLIEvents {
  // User input events
  userInput: { input: string; mode: CLIMode }
  cancelRequested: { reason: string }
  modeChanged: { previousMode: CLIMode; newMode: CLIMode }

  // System events
  ready: { startTime: Date }
  shutdown: { reason: string }
  error: { error: Error; context?: string }

  // UI events
  progressUpdate: { phase: string; progress: number; details?: string }
  messageReceived: { content: string; type: MessageType }
  streamingStarted: undefined
  streamingChunk: { content: string }
  streamingComplete: { totalTime: number }
  streamingCancelled: undefined
}

export type CLIMode = 'interactive' | 'command' | 'streaming'
export type MessageType = 'status' | 'streaming' | 'complete' | 'error' | 'info' | 'warning'

export interface CLIConfig {
  // Behavior
  enableHotkeys: boolean
  enableModeIndicator: boolean
  enableProgressDisplay: boolean
  enableStreaming: boolean

  // Appearance
  prompt: string
  colors: boolean
  animations: boolean

  // Input handling
  historySize: number
  autoComplete: boolean

  // Performance
  streamingThrottle: number // ms between characters
  maxBufferSize: number

  // State Management
  stateManager?: any // StateManager for UI updates
  messageQueue?: any // Message queue for communication

  // Debugging
  debug: boolean
}

export interface CLIState {
  mode: CLIMode
  isProcessing: boolean
  isStreamingActive: boolean
  currentInput: string
  history: string[]
  startTime: Date
  lastActivity: Date
}

/**
 * Main CLI Framework interface
 */
export interface ICLIFramework {
  // Lifecycle
  initialize(): Promise<void>
  start(): Promise<void>
  shutdown(): Promise<void>

  // State management
  getState(): CLIState
  setMode(mode: CLIMode): void
  getMode(): CLIMode

  // Input/Output
  showPrompt(): void
  handleInput(input: string): void
  displayMessage(content: string, type?: MessageType): void
  displayProgress(phase: string, progress: number, details?: string): void

  // Streaming
  startStreaming(): void
  addStreamingChunk(content: string): void
  completeStreaming(message?: string): void
  cancelStreaming(): void

  // State Management Integration
  subscribeToStateChanges(stateManager: any): void

  // Pure message-driven state reset (v-0.6.1)
  resetProcessingState?(): void

  // Configuration
  updateConfig(config: Partial<CLIConfig>): void
  getConfig(): CLIConfig
}

/**
 * Agent integration interface
 */
export interface IAgentCLIBridge {
  // Connect CLI to agent
  connectAgent(agent: any): void
  disconnectAgent(): void

  // Agent event handlers
  onAgentProgress(progress: { phase: string; progress: number; details?: string }): void
  onAgentMessage(message: { content: string; type: string }): void
  onAgentComplete(result: any): void
  onAgentError(error: any): void
  onAgentCancelled(reason: string): void

  // CLI to agent communication
  sendToAgent(input: string): void
  cancelAgent(): void
}

/**
 * Keyboard manager interface
 */
export interface IKeyboardManager {
  enable(): void
  disable(): void
  isEnabled(): boolean

  on(event: 'shiftTab' | 'escape' | 'ctrlC' | 'keypress', listener: (...args: any[]) => void): void
  off(event: string, listener: (...args: any[]) => void): void

  updateConfig(config: any): void
  destroy(): void
}

/**
 * UI component interfaces
 */
export interface IProgressDisplay {
  start(state?: any): void
  update(state: any): void
  complete(message?: string): void
  hide(): void
  isShowing(): boolean
  destroy(): void
}

export interface IModeIndicator {
  setMode(mode: CLIMode): void
  getMode(): CLIMode
  cycleMode(): CLIMode
  show(): void
  hide(): void
  getModeInfo(): any
  destroy(): void
}

export interface IStreamingRenderer {
  startStreaming(): void
  addChunk(chunk: { content: string; type: string }): void
  cancel(): void
  getState(): any
  destroy(): void
}
