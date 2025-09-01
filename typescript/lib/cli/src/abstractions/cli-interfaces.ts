/**
 * CLI-Specific Interfaces - Built on top of lib abstractions
 *
 * These interfaces extend the lib layer abstractions with CLI-specific concerns
 * like keyboard shortcuts, UI rendering, and terminal state management.
 */

// ============================================================================
// CLI State Management
// ============================================================================

export type AppState = 'busy' | 'ready'
export type AppSubState = 'planning' | 'editing' | 'generic'

export interface AppStateContext {
  readonly currentState: AppState
  readonly currentSubState: AppSubState
  readonly lastSubState: AppSubState
  readonly taskName?: string
  readonly startTime?: Date
}

export interface StateEvent {
  readonly type: string
  readonly payload?: unknown
}

// ============================================================================
// CLI Application Interface
// ============================================================================

export interface ICLIApplication {
  initialize(config: CLIConfig): Promise<void>
  start(): Promise<void>
  stop(): Promise<void>
  processInput(input: string): Promise<void>
  getStatus(): CLIStatus
  handleKeyboardShortcut(key: KeyboardShortcut): void
}

export interface CLIConfig {
  readonly framework: 'readline' | 'ink' | 'hybrid'
  readonly enableShellCommands?: boolean
  readonly sessionPersistence?: boolean
  readonly theme?: CLITheme
  readonly enableLangChainParsing?: boolean
  readonly customCommands?: any[]
}

export interface CLIStatus {
  readonly isRunning: boolean
  readonly currentState: AppState
  readonly currentSubState: AppSubState
  readonly uptime: number
  readonly commandsExecuted: number
  readonly errors: number
}

export interface KeyboardShortcut {
  readonly key: string
  readonly ctrl?: boolean
  readonly shift?: boolean
  readonly meta?: boolean
}

export interface CLITheme {
  readonly primary: string
  readonly secondary: string
  readonly success: string
  readonly error: string
  readonly warning: string
  readonly info: string
}

// ============================================================================
// State Management Interface
// ============================================================================

export interface IStateManager {
  getCurrentState(): AppState
  getCurrentSubState(): AppSubState
  getStateContext(): AppStateContext
  transition(event: StateEvent): void
  canCycleStates(): boolean
  cycleReadyStates(): void
  setBusy(taskName: string): void
  setReady(subState?: AppSubState): void
  subscribe(callback: StateCallback): () => void
  getMachineSnapshot(): StateMachineSnapshot
}

export type StateCallback = (context: AppStateContext, event?: StateEvent) => void

export interface StateMachineSnapshot {
  readonly value: string
  readonly context: AppStateContext
  readonly canTransition: (event: StateEvent) => boolean
}

// ============================================================================
// Framework Renderer Interface
// ============================================================================

export interface IFrameworkRenderer {
  render(state: AppStateContext): void
  handleInput(input: string): void
  showOutput(message: string, type?: 'info' | 'success' | 'error' | 'warning'): void
  showStateIndicator(state: AppState, subState?: AppSubState): void
  showBusyIndicator(taskName: string): void
  clear(): void
  cleanup(): void
}

// ============================================================================
// CLI Integration Types
// ============================================================================

// Types for CLI-to-lib integration (no re-exports needed)
// CLI should use lib layer directly via dependency injection
