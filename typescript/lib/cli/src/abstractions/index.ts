/**
 * CLI Abstractions - Export CLI-specific interfaces and types
 */

// Re-export commonly used types for convenience
export type {
  AppState,
  AppStateContext,
  AppSubState,
  CLIConfig,
  CLIStatus,
  ICLIApplication,
  IFrameworkRenderer,
  IStateManager,
  StateEvent,
} from './cli-interfaces'
// CLI-specific interfaces (built on top of lib abstractions)
export * from './cli-interfaces'
export type {
  CLIStateActor,
  CLIStateContext,
  CLIStateEvent,
} from './state-machine'
// State machine
export * from './state-machine'
