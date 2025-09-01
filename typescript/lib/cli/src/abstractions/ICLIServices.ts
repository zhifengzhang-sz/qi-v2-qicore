/**
 * CLI Services abstraction interfaces
 *
 * Defines interfaces for shared services that use QiCore patterns internally.
 * These services are framework-agnostic and used by all terminal implementations.
 */

import type { QiError, Result } from '@qi/base'

/**
 * Event manager interface for CLI event handling
 */
export interface IEventManager {
  /**
   * Subscribe to an event
   */
  on(event: string, listener: (...args: any[]) => void): void

  /**
   * Unsubscribe from an event
   */
  off(event: string, listener: (...args: any[]) => void): void

  /**
   * Emit an event
   */
  emit(event: string, ...args: any[]): boolean

  /**
   * Subscribe to an event once
   */
  once(event: string, listener: (...args: any[]) => void): void

  /**
   * Remove all listeners for an event
   */
  removeAllListeners(event?: string): void

  /**
   * Get current event listeners
   */
  getListeners(event: string): Function[]

  /**
   * Clean up resources
   */
  destroy(): void
}

/**
 * Command parsing result
 */
export interface CommandParseResult {
  type: 'command' | 'prompt'
  content: string
  command?: string
  args?: string[]
  flags?: Record<string, string | boolean>
}

/**
 * Command handler function type
 */
export type CommandHandler = (
  args: string[],
  flags: Record<string, string | boolean>
) => Promise<Result<string, QiError>>

/**
 * Command router interface for parsing and routing commands
 */
export interface ICommandRouter {
  /**
   * Parse input to determine if it's a command or prompt
   */
  parseInput(input: string): Result<CommandParseResult, QiError>

  /**
   * Handle a parsed command
   */
  handleCommand(
    command: string,
    args: string[],
    flags: Record<string, string | boolean>
  ): Promise<Result<string, QiError>>

  /**
   * Register a command handler
   */
  registerCommand(command: string, handler: CommandHandler): Result<void, QiError>

  /**
   * Unregister a command handler
   */
  unregisterCommand(command: string): Result<void, QiError>

  /**
   * Get list of registered commands
   */
  getRegisteredCommands(): string[]

  /**
   * Check if a command is registered
   */
  hasCommand(command: string): boolean

  /**
   * Get command help text
   */
  getCommandHelp(command?: string): string

  /**
   * Validate command syntax
   */
  validateCommand(command: string, args: string[]): Result<void, QiError>
}

/**
 * Agent connection interface
 */
export interface IAgentConnector {
  /**
   * Connect to an agent
   */
  connectAgent(agent: any): Result<void, QiError>

  /**
   * Disconnect from current agent
   */
  disconnectAgent(): Result<void, QiError>

  /**
   * Send input to connected agent
   */
  sendToAgent(input: string, context?: any): Promise<Result<void, QiError>>

  /**
   * Cancel current agent operation
   */
  cancelAgent(): Result<void, QiError>

  /**
   * Check if agent is connected
   */
  isAgentConnected(): boolean

  /**
   * Get current agent information
   */
  getAgentInfo(): any

  /**
   * Set up agent event handlers
   */
  onAgentProgress(callback: (progress: any) => void): void
  onAgentMessage(callback: (message: any) => void): void
  onAgentComplete(callback: (result: any) => void): void
  onAgentError(callback: (error: any) => void): void
  onAgentCancelled(callback: (reason: string) => void): void

  /**
   * Remove agent event handlers
   */
  removeAgentHandlers(): void
}

/**
 * Configuration manager interface
 */
export interface IConfigManager {
  /**
   * Load configuration from file
   */
  loadConfig(configPath: string): Promise<Result<any, QiError>>

  /**
   * Get configuration value
   */
  get<T = any>(key: string): Result<T, QiError>

  /**
   * Set configuration value
   */
  set<T = any>(key: string, value: T): Result<void, QiError>

  /**
   * Merge configuration
   */
  merge(config: any): Result<void, QiError>

  /**
   * Validate configuration
   */
  validate(): Result<void, QiError>

  /**
   * Get current configuration
   */
  getConfig(): any

  /**
   * Watch for configuration changes
   */
  watchConfig(callback: (config: any) => void): void
}

/**
 * State management interface
 */
export interface IStateManager {
  /**
   * Get current state
   */
  getState(): any

  /**
   * Update state
   */
  updateState(updates: any): Result<void, QiError>

  /**
   * Subscribe to state changes
   */
  subscribe(callback: (state: any, change: any) => void): () => void

  /**
   * Reset state to initial values
   */
  resetState(): Result<void, QiError>

  /**
   * Get state history
   */
  getStateHistory(): any[]

  /**
   * Validate state
   */
  validateState(state?: any): Result<void, QiError>
}
