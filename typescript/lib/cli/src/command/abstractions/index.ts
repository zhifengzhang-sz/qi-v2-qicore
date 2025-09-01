/**
 * Command Module - Public Abstractions
 *
 * Public interface contracts for the command module.
 * Other modules should only import from this file.
 */

export type {
  CommandDefinition,
  CommandExecutor,
  CommandHandlerConfig,
  CommandParameter,
  CommandRequest,
  CommandResult,
  ICommandHandler,
} from './ICommandHandler'
