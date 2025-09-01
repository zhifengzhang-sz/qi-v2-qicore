/**
 * Command Module - Implementation
 *
 * Concrete implementation of command handling.
 * Internal to command module - other modules cannot access this directly.
 */

import { exec } from 'node:child_process'
import { promisify } from 'node:util'
import type {
  CommandDefinition,
  CommandExecutor,
  CommandHandlerConfig,
  CommandRequest,
  CommandResult,
  ICommandHandler,
} from '../abstractions/index'

const execAsync = promisify(exec)

/**
 * Built-in command definitions
 */
const BUILT_IN_COMMANDS: CommandDefinition[] = [
  {
    name: 'help',
    description: 'Show available commands and their usage',
    usage: '/help [command_name]',
    aliases: ['h', '?'],
    category: 'system',
    parameters: [
      {
        name: 'command',
        type: 'string',
        required: false,
        description: 'Specific command to get help for',
      },
    ],
  },
  {
    name: 'status',
    description: 'Show current application status',
    usage: '/status',
    aliases: ['info'],
    category: 'system',
    parameters: [],
  },
  {
    name: 'clear',
    description: 'Clear the screen',
    usage: '/clear',
    aliases: ['cls'],
    category: 'ui',
    parameters: [],
  },
  {
    name: 'exit',
    description: 'Exit the application',
    usage: '/exit',
    aliases: ['quit', 'q'],
    category: 'system',
    parameters: [],
  },
  {
    name: 'model',
    description: 'Show or change the current LLM model',
    usage: '/model [model_name]',
    aliases: ['m'],
    category: 'llm',
    parameters: [
      {
        name: 'model_name',
        type: 'string',
        required: false,
        description: 'Model name to switch to',
      },
    ],
  },
  {
    name: 'provider',
    description: 'Show or change the current LLM provider',
    usage: '/provider [provider_name]',
    aliases: ['p'],
    category: 'llm',
    parameters: [
      {
        name: 'provider_name',
        type: 'string',
        required: false,
        description: 'Provider name to switch to',
      },
    ],
  },
]

/**
 * Safe shell commands that can be executed
 */
const SAFE_SHELL_COMMANDS = new Set([
  'ls',
  'dir',
  'pwd',
  'whoami',
  'date',
  'echo',
  'cat',
  'head',
  'tail',
  'grep',
  'find',
  'which',
  'man',
  'history',
  'ps',
  'top',
  'df',
  'du',
  'git',
  'npm',
  'bun',
  'node',
  'python',
  'pip',
])

/**
 * Command handler implementation
 */
export class CommandHandler implements ICommandHandler {
  private commandRegistry = new Map<string, CommandExecutor>()
  private commandDefinitions = new Map<string, CommandDefinition>()
  private aliasMap = new Map<string, string>()
  private config: CommandHandlerConfig

  constructor(config: CommandHandlerConfig = {}) {
    this.config = {
      enableBuiltInCommands: true,
      enableShellCommands: false,
      ...config,
    }

    if (this.config.enableBuiltInCommands) {
      this.registerBuiltInCommands()
    }

    if (this.config.customCommands) {
      for (const [name, executor] of this.config.customCommands) {
        this.registerCommand(
          {
            name,
            description: `Custom command: ${name}`,
            usage: `/${name}`,
            category: 'custom',
            parameters: [],
          },
          executor
        )
      }
    }
  }

  async executeCommand(request: CommandRequest): Promise<CommandResult> {
    const startTime = Date.now()

    try {
      // Resolve command name (handle aliases)
      const resolvedCommand = this.resolveCommandName(request.commandName)

      // Check if it's a registered command
      if (this.commandRegistry.has(resolvedCommand)) {
        const handler = this.commandRegistry.get(resolvedCommand)!
        const result = await handler(request)

        return {
          ...result,
          metadata: new Map([...result.metadata, ['executionTime', Date.now() - startTime]]),
        }
      }

      // Try shell command execution if enabled
      if (this.config.enableShellCommands && this.isSafeShellCommand(resolvedCommand)) {
        return await this.executeShellCommand(
          resolvedCommand,
          Array.from(request.parameters.values()) as string[]
        )
      }

      // Command not found
      return {
        status: 'not_found',
        content: `Command not found: ${request.commandName}. Use /help to see available commands.`,
        success: false,
        output: '',
        commandName: request.commandName,
        metadata: new Map([['executionTime', Date.now() - startTime]]),
      }
    } catch (error) {
      return {
        status: 'error',
        content: `Command execution failed: ${error instanceof Error ? error.message : String(error)}`,
        success: false,
        output: '',
        commandName: request.commandName,
        metadata: new Map([['executionTime', Date.now() - startTime]]),
      }
    }
  }

  getAvailableCommands(): readonly CommandDefinition[] {
    return Array.from(this.commandDefinitions.values())
  }

  async validateCommand(
    commandName: string,
    _parameters: ReadonlyMap<string, unknown>
  ): Promise<boolean> {
    const resolvedCommand = this.resolveCommandName(commandName)
    return this.commandRegistry.has(resolvedCommand)
  }

  registerCommand(definition: CommandDefinition, handler: CommandExecutor): void {
    // Validate command name
    if (!definition.name || !/^[a-zA-Z][a-zA-Z0-9_-]*$/.test(definition.name)) {
      throw new Error(`Invalid command name: ${definition.name}`)
    }

    // Register the handler and definition
    this.commandRegistry.set(definition.name, handler)
    this.commandDefinitions.set(definition.name, definition)

    // Register aliases
    if (definition.aliases) {
      for (const alias of definition.aliases) {
        this.aliasMap.set(alias, definition.name)
      }
    }
  }

  unregisterCommand(commandName: string): void {
    this.commandRegistry.delete(commandName)
    const definition = this.commandDefinitions.get(commandName)
    this.commandDefinitions.delete(commandName)

    // Remove aliases
    if (definition?.aliases) {
      for (const alias of definition.aliases) {
        this.aliasMap.delete(alias)
      }
    }
  }

  // Private methods

  private registerBuiltInCommands(): void {
    // Help command
    this.registerCommand(
      BUILT_IN_COMMANDS.find((c) => c.name === 'help')!,
      async (request: CommandRequest) => {
        const args = Array.from(request.parameters.values()) as string[]

        if (args.length > 0) {
          const help = this.getCommandHelp(args[0])
          return {
            status: 'success',
            content: help || `No help available for command: ${args[0]}`,
            output: help || `No help available for command: ${args[0]}`,
            commandName: 'help',
            success: true,
            metadata: new Map(),
          }
        }

        const commands = this.getAvailableCommands()
        let output = 'Available commands:\n\n'

        for (const cmd of commands) {
          output += `  /${cmd.name} - ${cmd.description}\n`
        }

        output += '\nUse /help <command> for detailed usage information.'

        return {
          status: 'success',
          content: output,
          output: output,
          commandName: 'help',
          success: true,
          metadata: new Map(),
        }
      }
    )

    // Status command
    this.registerCommand(
      BUILT_IN_COMMANDS.find((c) => c.name === 'status')!,
      async (_request: CommandRequest) => {
        const output = [
          `Commands Registered: ${this.commandRegistry.size}`,
          `Shell Commands: ${this.config.enableShellCommands ? 'Enabled' : 'Disabled'}`,
        ].join('\\n')

        return {
          status: 'success',
          content: output,
          output: output,
          commandName: 'status',
          success: true,
          metadata: new Map(),
        }
      }
    )

    // Clear command
    this.registerCommand(
      BUILT_IN_COMMANDS.find((c) => c.name === 'clear')!,
      async (_request: CommandRequest) => {
        // Clear screen using ANSI escape codes
        process.stdout.write('\\x1b[2J\\x1b[0f')
        return {
          status: 'success',
          content: 'Screen cleared',
          output: '',
          commandName: 'clear',
          success: true,
          metadata: new Map(),
        }
      }
    )

    // Exit command
    this.registerCommand(
      BUILT_IN_COMMANDS.find((c) => c.name === 'exit')!,
      async (_request: CommandRequest) => {
        // Initiate graceful shutdown
        process.stdout.write('\\n👋 Goodbye!\\n')

        // Use setTimeout to allow the message to be displayed before exiting
        setTimeout(() => {
          process.exit(0)
        }, 100)

        return {
          status: 'success',
          content: 'Exiting application...',
          output: 'Exiting application...',
          commandName: 'exit',
          success: true,
          metadata: new Map(),
        }
      }
    )
  }

  private resolveCommandName(name: string): string {
    return this.aliasMap.get(name) || name
  }

  private getCommandHelp(name: string): string | null {
    const resolved = this.resolveCommandName(name)
    const definition = this.commandDefinitions.get(resolved)

    if (!definition) return null

    let help = `${definition.name}: ${definition.description}\\n`
    help += `Usage: ${definition.usage}`

    if (definition.aliases?.length) {
      help += `\\nAliases: ${definition.aliases.join(', ')}`
    }

    return help
  }

  private isSafeShellCommand(command: string): boolean {
    return SAFE_SHELL_COMMANDS.has(command)
  }

  private async executeShellCommand(
    command: string,
    args: readonly string[]
  ): Promise<CommandResult> {
    const startTime = Date.now()

    try {
      // Sanitize arguments
      const sanitizedArgs = args.map((arg) => this.sanitizeShellArg(arg))
      const fullCommand = `${command} ${sanitizedArgs.join(' ')}`

      // Execute with timeout
      const { stdout, stderr } = await execAsync(fullCommand, {
        timeout: 30000, // 30 second timeout
        maxBuffer: 1024 * 1024, // 1MB buffer
      })

      return {
        status: 'success',
        content: stdout || stderr || 'Command executed successfully',
        output: stdout || stderr || 'Command executed successfully',
        commandName: command,
        success: true,
        metadata: new Map([['executionTime', Date.now() - startTime]]),
      }
    } catch (error: any) {
      return {
        status: 'error',
        content: error.message || 'Shell command execution failed',
        output: error.message || 'Shell command execution failed',
        commandName: command,
        success: false,
        metadata: new Map([
          ['executionTime', Date.now() - startTime],
          ['exitCode', error.code],
        ]),
      }
    }
  }

  private sanitizeShellArg(arg: string): string {
    // Basic sanitization - escape special characters
    return arg.replace(/[;&|`$(){}[\\]]/g, '\\\\$&')
  }
}
