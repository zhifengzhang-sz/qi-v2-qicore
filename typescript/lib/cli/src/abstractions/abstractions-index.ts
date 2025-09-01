/**
 * CLI Abstractions - Framework-agnostic interfaces
 *
 * This module exports all the core abstractions used throughout the CLI architecture.
 * These interfaces ensure that different terminal frameworks can be swapped seamlessly.
 */

// Dependency injection abstractions
export type {
  CLIFactory,
  ContainerConfig,
  ContainerFactory,
  FrameworkRegistration,
  ICLIContainer,
  ICLIContainerBuilder,
  IFrameworkRegistry,
  ServiceFactory,
  ServiceLifecycle,
  ServiceRegistration,
} from './ICLIContainer'
// Core framework interfaces (existing)
export type {
  CLIConfig,
  CLIEvents,
  CLIMode,
  CLIState,
  IAgentCLIBridge,
  ICLIFramework,
  IKeyboardManager,
  IModeIndicator,
  IProgressDisplay,
  IStreamingRenderer,
  MessageType,
} from './ICLIFramework'
// Service abstractions (QiCore-based services)
export type {
  CommandHandler,
  CommandParseResult,
  IAgentConnector,
  ICommandRouter,
  IConfigManager,
  IEventManager,
  IStateManager,
} from './ICLIServices'
// Input management abstraction
export type {
  IInputManager,
  InputConfig,
  KeypressData,
} from './IInputManager'
// Terminal abstraction
export type {
  ITerminal,
  TerminalDimensions,
} from './ITerminal'
// UI component abstractions
export type {
  IMessageDisplay,
  IModeRenderer,
  IProgressRenderer,
  IStreamRenderer,
  IUIComponent,
  ProgressConfig,
} from './IUIComponent'
