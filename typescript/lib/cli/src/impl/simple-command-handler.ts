/**
 * Simple CLI Command Handler
 *
 * Focused on CLI-specific commands only (help, exit, clear, etc.)
 * Does NOT handle agent work like classification or LLM calls.
 */

export interface CLICommand {
  readonly name: string
  readonly description: string
  readonly usage: string
  readonly aliases?: readonly string[]
}

export interface CLICommandResult {
  readonly success: boolean
  readonly output: string
  readonly shouldExit?: boolean
  readonly shouldClear?: boolean
}

export interface CLICommandRequest {
  readonly command: string
  readonly args: readonly string[]
  readonly rawInput: string
}

/**
 * Simple command handler for basic CLI operations
 */
export class SimpleCLICommandHandler {
  private commands: Map<string, CLICommand> = new Map()
  private aliases: Map<string, string> = new Map()

  constructor() {
    this.registerBuiltInCommands()
  }

  private registerBuiltInCommands(): void {
    const builtInCommands: CLICommand[] = [
      {
        name: 'help',
        description: 'Show available CLI commands',
        usage: '/help [command_name]',
        aliases: ['h', '?'],
      },
      {
        name: 'exit',
        description: 'Exit the CLI application',
        usage: '/exit',
        aliases: ['quit', 'q'],
      },
      {
        name: 'clear',
        description: 'Clear the screen',
        usage: '/clear',
        aliases: ['cls'],
      },
      {
        name: 'version',
        description: 'Show CLI version information',
        usage: '/version',
        aliases: ['v'],
      },
    ]

    for (const cmd of builtInCommands) {
      this.commands.set(cmd.name, cmd)
      if (cmd.aliases) {
        for (const alias of cmd.aliases) {
          this.aliases.set(alias, cmd.name)
        }
      }
    }
  }

  /**
   * Execute a CLI command
   */
  executeCommand(request: CLICommandRequest): CLICommandResult {
    const commandName = this.aliases.get(request.command) || request.command
    const command = this.commands.get(commandName)

    if (!command) {
      return {
        success: false,
        output: `Unknown command: /${request.command}. Type /help for available commands.`,
      }
    }

    switch (commandName) {
      case 'help':
        return this.handleHelp(request.args)
      case 'exit':
        return this.handleExit()
      case 'clear':
        return this.handleClear()
      case 'version':
        return this.handleVersion()
      default:
        return {
          success: false,
          output: `Command /${commandName} is not implemented yet.`,
        }
    }
  }

  /**
   * Check if input is a CLI command
   */
  isCommand(input: string): boolean {
    return input.trim().startsWith('/')
  }

  /**
   * Parse command input into request
   */
  parseCommand(input: string): CLICommandRequest | null {
    const trimmed = input.trim()
    if (!trimmed.startsWith('/')) {
      return null
    }

    const parts = trimmed.slice(1).split(/\s+/)
    const command = parts[0] || ''
    const args = parts.slice(1)

    return {
      command,
      args,
      rawInput: input,
    }
  }

  /**
   * Get all available commands
   */
  getCommands(): readonly CLICommand[] {
    return Array.from(this.commands.values())
  }

  private handleHelp(args: readonly string[]): CLICommandResult {
    if (args.length > 0 && args[0]) {
      const commandName = this.aliases.get(args[0]) || args[0]
      const command = this.commands.get(commandName)

      if (command) {
        let output = `Command: /${command.name}\n`
        output += `Description: ${command.description}\n`
        output += `Usage: ${command.usage}`
        if (command.aliases && command.aliases.length > 0) {
          output += `\nAliases: ${command.aliases.map((a) => `/${a}`).join(', ')}`
        }
        return { success: true, output }
      } else {
        return { success: false, output: `Unknown command: /${args[0]}` }
      }
    }

    // Show all commands
    let output = 'Available CLI Commands:\n\n'
    const commands = Array.from(this.commands.values())

    for (const cmd of commands) {
      output += `  /${cmd.name}`
      if (cmd.aliases && cmd.aliases.length > 0) {
        output += ` (${cmd.aliases.map((a) => `/${a}`).join(', ')})`
      }
      output += `\n    ${cmd.description}\n    Usage: ${cmd.usage}\n\n`
    }

    output += 'Note: This CLI only handles interface commands.\n'
    output += 'For AI agent interactions, use the agent system directly.'

    return { success: true, output }
  }

  private handleExit(): CLICommandResult {
    return {
      success: true,
      output: 'Goodbye!',
      shouldExit: true,
    }
  }

  private handleClear(): CLICommandResult {
    return {
      success: true,
      output: '',
      shouldClear: true,
    }
  }

  private handleVersion(): CLICommandResult {
    return {
      success: true,
      output: 'qi-v2-agent CLI v0.2.7\nFocused CLI interface - agent work handled separately',
    }
  }
}
