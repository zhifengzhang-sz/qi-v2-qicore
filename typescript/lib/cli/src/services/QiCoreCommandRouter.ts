/**
 * QiCore-based Command Router implementation
 *
 * Provides robust command parsing and routing with qicore Result<T> patterns
 * and functional composition for validation and execution.
 */

import {
  create,
  Err,
  flatMap,
  match,
  Ok,
  type QiError,
  type Result,
  validationError,
} from '@qi/base'
import type {
  CommandHandler,
  CommandParseResult,
  ICommandRouter,
} from '../abstractions/ICLIServices'

/**
 * Command router error types
 */
interface CommandRouterError extends QiError {
  context: {
    command?: string
    input?: string
    args?: string[]
    operation?: string
    availableCommands?: string[]
  }
}

const commandError = (
  code: string,
  message: string,
  context: CommandRouterError['context'] = {}
): CommandRouterError => create(code, message, 'VALIDATION', context) as CommandRouterError

/**
 * Command information for help and validation
 */
interface CommandInfo {
  handler: CommandHandler
  description: string
  usage: string
  examples?: string[]
  minArgs?: number
  maxArgs?: number
}

/**
 * QiCore implementation of command routing
 * Provides robust parsing, validation, and execution with proper error handling
 */
export class QiCoreCommandRouter implements ICommandRouter {
  private commands: Map<string, CommandInfo> = new Map()
  private aliases: Map<string, string> = new Map()
  private commandPrefix: string = '/'

  constructor(options: { commandPrefix?: string } = {}) {
    this.commandPrefix = options.commandPrefix || '/'
    this.registerBuiltinCommands()
  }

  /**
   * Parse input to determine if it's a command or prompt
   */
  parseInput(input: string): Result<CommandParseResult, QiError> {
    const validationResult = this.validateInput(input)

    return flatMap((validInput: string) => this.parseValidInput(validInput), validationResult)
  }

  /**
   * Handle a parsed command
   */
  async handleCommand(
    command: string,
    args: string[],
    flags: Record<string, string | boolean>
  ): Promise<Result<string, QiError>> {
    const validationResult = this.validateCommandExecution(command, args)

    return await match(
      async () => {
        const commandInfo = this.getCommandInfo(command)
        if (!commandInfo) {
          return Err(
            commandError('COMMAND_NOT_FOUND', `Command '${command}' not found`, {
              command,
              availableCommands: this.getRegisteredCommands(),
            })
          )
        }

        try {
          return await commandInfo.handler(args, flags)
        } catch (error) {
          return Err(
            commandError(
              'COMMAND_EXECUTION_FAILED',
              `Command execution failed: ${error instanceof Error ? error.message : 'Unknown error'}`,
              { command, args, operation: 'execute' }
            )
          )
        }
      },
      async (error: QiError) => Err(error),
      validationResult
    )
  }

  /**
   * Register a command handler
   */
  registerCommand(command: string, handler: CommandHandler): Result<void, QiError> {
    const validationResult = this.validateCommandName(command)

    return match(
      () => {
        const info: CommandInfo = {
          handler,
          description: `Custom command: ${command}`,
          usage: `${this.commandPrefix}${command} [args...]`,
        }

        this.commands.set(command, info)
        return Ok(void 0)
      },
      (error): Result<void, QiError> => Err(error),
      validationResult
    )
  }

  /**
   * Register a command with full metadata
   */
  registerCommandWithInfo(
    command: string,
    handler: CommandHandler,
    info: Partial<Omit<CommandInfo, 'handler'>>
  ): Result<void, QiError> {
    const validationResult = this.validateCommandName(command)

    return match(
      () => {
        const fullInfo: CommandInfo = {
          handler,
          description: info.description || `Command: ${command}`,
          usage: info.usage || `${this.commandPrefix}${command} [args...]`,
          examples: info.examples || [],
          minArgs: info.minArgs || 0,
          maxArgs: info.maxArgs || Infinity,
        }

        this.commands.set(command, fullInfo)
        return Ok(void 0)
      },
      (error): Result<void, QiError> => Err(error),
      validationResult
    )
  }

  /**
   * Register a command alias
   */
  registerAlias(alias: string, targetCommand: string): Result<void, QiError> {
    const aliasValidation = this.validateCommandName(alias)

    return flatMap(() => {
      if (!this.commands.has(targetCommand)) {
        return Err(
          commandError(
            'TARGET_COMMAND_NOT_FOUND',
            `Target command '${targetCommand}' does not exist`,
            { command: targetCommand, operation: 'registerAlias' }
          )
        )
      }

      this.aliases.set(alias, targetCommand)
      return Ok(void 0)
    }, aliasValidation)
  }

  /**
   * Unregister a command handler
   */
  unregisterCommand(command: string): Result<void, QiError> {
    const validationResult = this.validateCommandName(command)

    return match(
      () => {
        if (!this.commands.has(command)) {
          return Err(
            commandError('COMMAND_NOT_FOUND', `Command '${command}' is not registered`, {
              command,
              operation: 'unregister',
            })
          )
        }

        this.commands.delete(command)

        // Remove any aliases pointing to this command
        for (const [alias, target] of this.aliases.entries()) {
          if (target === command) {
            this.aliases.delete(alias)
          }
        }

        return Ok(void 0)
      },
      (error) => Err(error),
      validationResult
    )
  }

  /**
   * Get list of registered commands
   */
  getRegisteredCommands(): string[] {
    return Array.from(this.commands.keys()).sort()
  }

  /**
   * Check if a command is registered
   */
  hasCommand(command: string): boolean {
    const normalizedCommand = this.resolveAlias(command)
    return this.commands.has(normalizedCommand)
  }

  /**
   * Get command help text
   */
  getCommandHelp(command?: string): string {
    if (command) {
      const normalizedCommand = this.resolveAlias(command)
      const info = this.commands.get(normalizedCommand)

      if (!info) {
        return `Command '${command}' not found.`
      }

      let help = `${info.description}\n\nUsage: ${info.usage}`

      if (info.examples && info.examples.length > 0) {
        help += `\n\nExamples:\n${info.examples.map((ex) => `  ${ex}`).join('\n')}`
      }

      return help
    }

    // General help
    let help = 'Available commands:\n\n'

    for (const [cmd, info] of this.commands.entries()) {
      help += `  ${this.commandPrefix}${cmd.padEnd(12)} ${info.description}\n`
    }

    if (this.aliases.size > 0) {
      help += '\nAliases:\n\n'
      for (const [alias, target] of this.aliases.entries()) {
        help += `  ${this.commandPrefix}${alias.padEnd(12)} → ${this.commandPrefix}${target}\n`
      }
    }

    help += `\nUse '${this.commandPrefix}help <command>' for detailed information about a specific command.`

    return help
  }

  /**
   * Validate command syntax
   */
  validateCommand(command: string, args: string[]): Result<void, QiError> {
    const normalizedCommand = this.resolveAlias(command)
    const info = this.commands.get(normalizedCommand)

    if (!info) {
      return Err(
        commandError('COMMAND_NOT_FOUND', `Command '${command}' not found`, {
          command,
          availableCommands: this.getRegisteredCommands(),
        })
      )
    }

    // Validate argument count
    if (info.minArgs !== undefined && args.length < info.minArgs) {
      return Err(
        commandError(
          'INSUFFICIENT_ARGS',
          `Command '${command}' requires at least ${info.minArgs} arguments, got ${args.length}`,
          { command, args, operation: 'validateArgs' }
        )
      )
    }

    if (info.maxArgs !== undefined && args.length > info.maxArgs) {
      return Err(
        commandError(
          'TOO_MANY_ARGS',
          `Command '${command}' accepts at most ${info.maxArgs} arguments, got ${args.length}`,
          { command, args, operation: 'validateArgs' }
        )
      )
    }

    return Ok(void 0)
  }

  // Private methods

  private validateInput(input: string): Result<string, QiError> {
    if (!input || typeof input !== 'string') {
      return Err(validationError('Input must be a non-empty string'))
    }

    const trimmed = input.trim()
    if (trimmed.length === 0) {
      return Err(validationError('Input cannot be empty'))
    }

    return Ok(trimmed)
  }

  private parseValidInput(input: string): Result<CommandParseResult, QiError> {
    if (!input.startsWith(this.commandPrefix)) {
      return Ok({
        type: 'prompt',
        content: input,
      })
    }

    // Parse command
    const commandLine = input.slice(this.commandPrefix.length)
    const parts = this.parseCommandLine(commandLine)

    if (parts.length === 0) {
      return Err(
        commandError('EMPTY_COMMAND', 'Command cannot be empty', { input, operation: 'parse' })
      )
    }

    const [command, ...args] = parts
    const { cleanArgs, flags } = this.separateArgsAndFlags(args)

    return Ok({
      type: 'command',
      content: input,
      command,
      args: cleanArgs,
      flags,
    })
  }

  private parseCommandLine(commandLine: string): string[] {
    const parts: string[] = []
    let current = ''
    let inQuotes = false
    let quoteChar = ''

    for (let i = 0; i < commandLine.length; i++) {
      const char = commandLine[i]

      if (!inQuotes && (char === '"' || char === "'")) {
        inQuotes = true
        quoteChar = char
      } else if (inQuotes && char === quoteChar) {
        inQuotes = false
        quoteChar = ''
      } else if (!inQuotes && char === ' ') {
        if (current) {
          parts.push(current)
          current = ''
        }
      } else {
        current += char
      }
    }

    if (current) {
      parts.push(current)
    }

    return parts
  }

  private separateArgsAndFlags(args: string[]): {
    cleanArgs: string[]
    flags: Record<string, string | boolean>
  } {
    const cleanArgs: string[] = []
    const flags: Record<string, string | boolean> = {}

    for (let i = 0; i < args.length; i++) {
      const arg = args[i]
      
      if (!arg) {
        continue // Skip undefined/empty args
      }

      if (arg.startsWith('--')) {
        // Long flag
        const flagName = arg.slice(2)
        if (flagName.includes('=')) {
          const [name, value] = flagName.split('=', 2)
          if (name && value !== undefined) {
            flags[name] = value
          }
        } else {
          const nextArg = args[i + 1]
          if (i + 1 < args.length && nextArg && !nextArg.startsWith('-')) {
            flags[flagName] = nextArg
            i++ // Skip next argument as it's the flag value
          } else {
            flags[flagName] = true
          }
        }
      } else if (arg.startsWith('-') && arg.length > 1) {
        // Short flag(s)
        const shortFlags = arg.slice(1)
        for (const flag of shortFlags) {
          flags[flag] = true
        }
      } else {
        cleanArgs.push(arg)
      }
    }

    return { cleanArgs, flags }
  }

  private validateCommandName(command: string): Result<void, QiError> {
    if (!command || typeof command !== 'string') {
      return Err(
        commandError('INVALID_COMMAND_NAME', 'Command name must be a non-empty string', {
          command,
          operation: 'validateName',
        })
      )
    }

    if (!/^[a-zA-Z][a-zA-Z0-9-_]*$/.test(command)) {
      return Err(
        commandError(
          'INVALID_COMMAND_FORMAT',
          'Command name must start with a letter and contain only letters, numbers, hyphens, and underscores',
          { command, operation: 'validateName' }
        )
      )
    }

    return Ok(void 0)
  }

  private validateCommandExecution(command: string, args: string[]): Result<void, QiError> {
    const nameValidation = this.validateCommandName(command)

    return flatMap(() => this.validateCommand(command, args), nameValidation)
  }

  private resolveAlias(command: string): string {
    return this.aliases.get(command) || command
  }

  private getCommandInfo(command: string): CommandInfo | undefined {
    const normalizedCommand = this.resolveAlias(command)
    return this.commands.get(normalizedCommand)
  }

  private registerBuiltinCommands(): void {
    // Help command
    this.registerCommandWithInfo(
      'help',
      async (args) => {
        const helpText = this.getCommandHelp(args[0])
        return Ok(helpText)
      },
      {
        description: 'Show help information for commands',
        usage: `${this.commandPrefix}help [command]`,
        examples: [`${this.commandPrefix}help`, `${this.commandPrefix}help status`],
        maxArgs: 1,
      }
    )

    // List commands
    this.registerCommandWithInfo(
      'commands',
      async () => {
        const commands = this.getRegisteredCommands()
        return Ok(`Available commands: ${commands.join(', ')}`)
      },
      {
        description: 'List all available commands',
        usage: `${this.commandPrefix}commands`,
        maxArgs: 0,
      }
    )

    // Status command
    this.registerCommandWithInfo(
      'status',
      async () => {
        const status = {
          mode: 'interactive',
          provider: 'ollama',
          model: 'qwen3:8b',
          uptime: process.uptime(),
          commands: this.getRegisteredCommands().length,
        }

        let statusText = '📊 System Status:\n\n'
        statusText += `  Mode: ${status.mode}\n`
        statusText += `  Provider: ${status.provider}\n`
        statusText += `  Model: ${status.model}\n`
        statusText += `  Uptime: ${Math.floor(status.uptime)}s\n`
        statusText += `  Commands: ${status.commands}`

        return Ok(statusText)
      },
      {
        description: 'Show system status and configuration',
        usage: `${this.commandPrefix}status`,
        maxArgs: 0,
      }
    )

    // Model command
    this.registerCommandWithInfo(
      'model',
      async (args) => {
        if (args.length === 0) {
          return Ok('Current model: qwen3:8b\nAvailable models: qwen3:8b, llama3.2:3b')
        }

        const newModel = args[0]
        // In a real implementation, this would update the model configuration
        return Ok(`Model would be changed to: ${newModel} (not implemented yet)`)
      },
      {
        description: 'Show or change the current LLM model',
        usage: `${this.commandPrefix}model [model_name]`,
        examples: [`${this.commandPrefix}model`, `${this.commandPrefix}model llama3.2:3b`],
        maxArgs: 1,
      }
    )
  }
}
