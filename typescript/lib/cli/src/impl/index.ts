/**
 * CLI Implementation - Export all concrete implementations
 */

// Parser implementations removed - CLI should delegate to lib layer InputClassifier

// v-0.6.1: Pure CLI implementation removed - event-based
// export { type CLIFeedback, type CLIInput, createPureCLI, type ICLI, PureCLI } from './pure-cli';

// Command handler implementations
// export { CLICommandHandler, createCommandHandler, createCommandHandlerWithShell } from './command-handler' // DEPRECATED: Mixes CLI/agent concerns
export { SimpleCLICommandHandler } from './simple-command-handler'
// State manager implementation
export { CLIStateManager, createDebugStateManager, createStateManager } from './state-manager'

// Workflow handler implementation - moved to qi-code
// export { CLIWorkflowHandler, createWorkflowHandler, createTestWorkflowHandler } from './workflow-handler'
// export type { IWorkflowHandler, WorkflowExecutionResult, WorkflowStreamUpdate } from './workflow-handler'

// Re-export types for convenience
export type * from '../abstractions/index'
export type * from '../abstractions/state-machine'
