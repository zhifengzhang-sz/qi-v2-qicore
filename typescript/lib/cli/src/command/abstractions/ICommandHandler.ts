/**
 * Command Module - Abstract Interfaces
 *
 * Defines contracts for command processing without any implementation details.
 * Other modules can only access commands through these interfaces.
 */

/**
 * Command execution request
 */
export interface CommandRequest {
  readonly commandName: string
  readonly parameters: ReadonlyMap<string, unknown>
  readonly rawInput: string
  readonly context?: ReadonlyMap<string, unknown>
}

/**
 * Command execution result
 */
export interface CommandResult {
  readonly status: 'success' | 'error' | 'not_found'
  readonly content: string
  readonly output: string
  readonly commandName: string
  readonly success: boolean
  readonly metadata: ReadonlyMap<string, unknown>
  readonly error?: string
}

/**
 * Command definition structure
 */
export interface CommandDefinition {
  readonly name: string
  readonly description: string
  readonly usage: string
  readonly aliases?: readonly string[]
  readonly category: string
  readonly parameters: readonly CommandParameter[]
}

/**
 * Command parameter definition
 */
export interface CommandParameter {
  readonly name: string
  readonly type: 'string' | 'number' | 'boolean' | 'array'
  readonly required: boolean
  readonly description: string
  readonly defaultValue?: unknown
}

/**
 * Command executor function type
 */
export type CommandExecutor = (request: CommandRequest) => Promise<CommandResult>

/**
 * Command handler configuration
 */
export interface CommandHandlerConfig {
  readonly enableBuiltInCommands?: boolean
  readonly enableShellCommands?: boolean
  readonly customCommands?: ReadonlyMap<string, CommandExecutor>
}

/**
 * Abstract command handler interface
 */
export interface ICommandHandler {
  /**
   * Execute a command
   */
  executeCommand(request: CommandRequest): Promise<CommandResult>

  /**
   * Get all available commands
   */
  getAvailableCommands(): readonly CommandDefinition[]

  /**
   * Validate if a command exists and parameters are valid
   */
  validateCommand(commandName: string, parameters: ReadonlyMap<string, unknown>): Promise<boolean>

  /**
   * Register a new command
   */
  registerCommand(definition: CommandDefinition, handler: CommandExecutor): void

  /**
   * Unregister a command
   */
  unregisterCommand(commandName: string): void
}
