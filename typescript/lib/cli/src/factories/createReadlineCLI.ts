/**
 * Readline CLI Factory
 *
 * Creates a complete CLI instance using the readline framework (zero dependencies)
 * with all necessary services and components properly wired up.
 */

import { create, Err, flatMap, match, Ok, type QiError, type Result } from '@qi/base'
import type { ICommandHandler } from '../command/abstractions/index'
import type { QiAsyncMessageQueue } from '@qi/amsg'
import { QiAsyncMessageQueue as QiAsyncMessageQueueImpl } from '@qi/amsg'
import type { QiMessage } from '@qi/amsg'
import type { CLIConfig, ICLIFramework } from '../abstractions/ICLIFramework'
import type { ICommandRouter } from '../abstractions/ICLIServices'
import type { IInputManager } from '../abstractions/IInputManager'
// Import interface types for dependency resolution
import type { ITerminal } from '../abstractions/ITerminal'
import type {
  IModeRenderer,
  IProgressRenderer,
  IStreamRenderer,
} from '../abstractions/IUIComponent'
import { CLIContainer } from '../container/CLIContainer'
// Readline framework implementations
import {
  ReadlineInputManager,
  ReadlineModeRenderer,
  ReadlineProgressRenderer,
  ReadlineStreamRenderer,
  ReadlineTerminal,
} from '../frameworks/readline/index'
// v-0.6.1: Pure message-driven CLI implementation
import { MessageDrivenCLI } from '../impl/MessageDrivenCLI'
// Shared QiCore services
import {
  // QiCoreAgentConnector, // v-0.6.1: Event-based service removed
  QiCoreCommandRouter,
  // QiCoreEventManager, // v-0.6.1: Event-based service removed
} from '../services/index'

/**
 * Factory error types
 */
interface FactoryError extends QiError {
  context: {
    operation?: string
    framework?: string
    serviceKey?: string
    config?: any
  }
}

const factoryError = (
  code: string,
  message: string,
  context: FactoryError['context'] = {}
): FactoryError => create(code, message, 'SYSTEM', context) as FactoryError

/**
 * Default CLI configuration for readline framework
 */
const DEFAULT_READLINE_CONFIG: CLIConfig = {
  enableHotkeys: true,
  enableModeIndicator: true,
  enableProgressDisplay: true,
  enableStreaming: true,
  prompt: '> ',
  colors: true,
  animations: true,
  historySize: 100,
  autoComplete: false,
  streamingThrottle: 0,
  maxBufferSize: 10000,
  debug: false,
}

/**
 * Create a CLI instance using the readline framework
 *
 * This factory assembles all the necessary components:
 * - Readline-based terminal and input management
 * - QiCore services for robust error handling
 * - Proper dependency injection and lifecycle management
 */
export function createReadlineCLI(
  config: Partial<CLIConfig> = {},
  options: {
    commandHandler?: ICommandHandler
    messageQueue?: QiAsyncMessageQueue<QiMessage> // v-0.6.1: Required for message-driven architecture
  } = {}
): Result<ICLIFramework, QiError> {
  const fullConfig: CLIConfig = { ...DEFAULT_READLINE_CONFIG, ...config }

  // Create container
  const containerResult = createContainer()

  return flatMap((container: CLIContainer) => {
    // Register all services
    const registrationResult = registerServices(container, fullConfig, options)

    return match(
      () => {
        // Create CLI instance with commandHandler and messageQueue options
        return createCLIInstance(
          container,
          fullConfig,
          options.commandHandler,
          options.messageQueue
        )
      },
      (error) => Err(error),
      registrationResult
    )
  }, containerResult)
}

/**
 * Create and configure the dependency injection container
 */
function createContainer(): Result<CLIContainer, QiError> {
  try {
    const container = new CLIContainer({
      enableDebug: false,
      enableValidation: true,
      maxResolutionDepth: 10,
    })

    return Ok(container)
  } catch (error) {
    return Err(
      factoryError(
        'CONTAINER_CREATION_FAILED',
        `Failed to create container: ${error instanceof Error ? error.message : 'Unknown error'}`,
        { operation: 'createContainer', framework: 'readline' }
      )
    )
  }
}

/**
 * Register all necessary services in the container
 */
function registerServices(
  container: CLIContainer,
  config: CLIConfig,
  options: {
    commandHandler?: ICommandHandler
    messageQueue?: QiAsyncMessageQueue<QiMessage>
  } = {}
): Result<void, QiError> {
  // Register terminal implementation
  const terminalResult = container.register('terminal', () => new ReadlineTerminal(), {
    singleton: true,
  })

  if (terminalResult.tag === 'failure') {
    return terminalResult
  }

  // Register message queue if provided (v-0.6.1: Required for message-driven architecture)
  if (options.messageQueue) {
    const messageQueueResult = container.register('messageQueue', () => options.messageQueue!, {
      singleton: true,
    })

    if (messageQueueResult.tag === 'failure') {
      return messageQueueResult
    }
  }

  // Register input manager
  const inputResult = container.register(
    'inputManager',
    () => {
      // v-0.6.1: ReadlineInputManager requires message queue
      const messageQueueResult = container.resolve('messageQueue')
      if (messageQueueResult.tag === 'failure') {
        throw new Error('Failed to resolve messageQueue')
      }
      return new ReadlineInputManager(messageQueueResult.value as any)
    },
    { singleton: true, destroyer: (instance) => (instance as ReadlineInputManager).close() }
  )

  if (inputResult.tag === 'failure') {
    return inputResult
  }

  // Register progress renderer
  const progressResult = container.register(
    'progressRenderer',
    () =>
      new ReadlineProgressRenderer({
        animated: config.animations,
        showElapsed: true,
        showPercentage: true,
      }),
    { singleton: true, destroyer: (instance) => (instance as ReadlineProgressRenderer).destroy() }
  )

  if (progressResult.tag === 'failure') {
    return progressResult
  }

  // Register mode renderer
  const modeResult = container.register(
    'modeRenderer',
    () =>
      new ReadlineModeRenderer({
        showIcon: config.colors,
        showLabel: false,
        position: 'inline',
      }),
    { singleton: true, destroyer: (instance) => (instance as ReadlineModeRenderer).destroy() }
  )

  if (modeResult.tag === 'failure') {
    return modeResult
  }

  // Register stream renderer
  const streamResult = container.register(
    'streamRenderer',
    () =>
      new ReadlineStreamRenderer({
        throttleMs: config.streamingThrottle,
        showCursor: config.animations,
        bufferSize: config.maxBufferSize,
      }),
    { singleton: true, destroyer: (instance) => (instance as ReadlineStreamRenderer).destroy() }
  )

  if (streamResult.tag === 'failure') {
    return streamResult
  }

  // Register shared QiCore services
  // v-0.6.1: Event manager removed - pure message-driven
  // const eventManagerResult = container.register(
  //   'eventManager',
  //   () => new QiCoreEventManager({ trackHistory: config.debug }),
  //   { singleton: true, destroyer: (instance) => (instance as QiCoreEventManager).destroy() }
  // );

  // v-0.6.1: Event manager result check removed
  // if (eventManagerResult.tag === 'failure') {
  //   return eventManagerResult;
  // }

  // Use QiCoreCommandRouter - if commandHandler provided, it will be used directly by CLI
  const commandRouterResult = container.register('commandRouter', () => new QiCoreCommandRouter(), {
    singleton: true,
  })

  if (commandRouterResult.tag === 'failure') {
    return commandRouterResult
  }

  // v-0.6.1: Agent connector removed - pure message-driven
  // const agentConnectorResult = container.register(
  //   'agentConnector',
  //   () => new QiCoreAgentConnector(),
  //   {
  //     singleton: true,
  //     destroyer: (instance) => {
  //       (instance as QiCoreAgentConnector).dispose();
  //     },
  //   }
  // );

  // v-0.6.1: Agent connector result check removed
  // if (agentConnectorResult.tag === 'failure') {
  //   return agentConnectorResult;
  // }

  return Ok(void 0)
}

/**
 * Create the CLI instance with resolved dependencies
 */
function createCLIInstance(
  container: CLIContainer,
  config: CLIConfig,
  commandHandler?: ICommandHandler,
  messageQueue?: QiAsyncMessageQueue<QiMessage>
): Result<ICLIFramework, QiError> {
  try {
    // v-0.6.1: Create default message queue if not provided
    const actualMessageQueue =
      messageQueue ||
      new QiAsyncMessageQueueImpl<QiMessage>({
        maxConcurrent: 1,
        priorityQueuing: true,
        autoCleanup: true,
        enableStats: false,
        messageTtl: 30000,
      })

    // Resolve all dependencies with explicit types
    const terminal = container.resolve<ITerminal>('terminal')
    const inputManager = container.resolve<IInputManager>('inputManager')
    const progressRenderer = container.resolve<IProgressRenderer>('progressRenderer')
    const modeRenderer = container.resolve<IModeRenderer>('modeRenderer')
    const streamRenderer = container.resolve<IStreamRenderer>('streamRenderer')
    const commandRouter = container.resolve<ICommandRouter>('commandRouter')

    // Check all resolutions succeeded
    const dependencies = [
      terminal,
      inputManager,
      progressRenderer,
      modeRenderer,
      streamRenderer,
      commandRouter,
    ]

    for (const dep of dependencies) {
      if (dep.tag === 'failure') {
        return dep as Result<ICLIFramework, QiError>
      }
    }

    // Create CLI instance with resolved dependencies
    // Type assertion is safe here since we already checked all deps succeeded
    const cli = new MessageDrivenCLI(
      (terminal as any).value,
      (inputManager as any).value,
      (progressRenderer as any).value,
      (modeRenderer as any).value,
      (streamRenderer as any).value,
      (commandRouter as any).value,
      actualMessageQueue, // v-0.6.1: Use message queue instead of agentConnector
      config,
      commandHandler // Pass the commandHandler directly
    )

    return Ok(cli)
  } catch (error) {
    return Err(
      factoryError(
        'CLI_CREATION_FAILED',
        `Failed to create CLI instance: ${error instanceof Error ? error.message : 'Unknown error'}`,
        { operation: 'createCLI', framework: 'readline' }
      )
    )
  }
}

/**
 * Validate CLI configuration for readline framework
 */
function validateReadlineConfig(config: CLIConfig): Result<void, QiError> {
  if (config.historySize < 0 || config.historySize > 10000) {
    return Err(
      factoryError('INVALID_HISTORY_SIZE', 'History size must be between 0 and 10000', {
        operation: 'validateConfig',
        config: { historySize: config.historySize },
      })
    )
  }

  if (config.streamingThrottle < 0 || config.streamingThrottle > 1000) {
    return Err(
      factoryError(
        'INVALID_STREAMING_THROTTLE',
        'Streaming throttle must be between 0 and 1000ms',
        { operation: 'validateConfig', config: { streamingThrottle: config.streamingThrottle } }
      )
    )
  }

  if (config.maxBufferSize < 100 || config.maxBufferSize > 100000) {
    return Err(
      factoryError('INVALID_BUFFER_SIZE', 'Max buffer size must be between 100 and 100000', {
        operation: 'validateConfig',
        config: { maxBufferSize: config.maxBufferSize },
      })
    )
  }

  return Ok(void 0)
}

/**
 * Create readline CLI with validation
 */
export function createValidatedReadlineCLI(
  config: Partial<CLIConfig> = {}
): Result<ICLIFramework, QiError> {
  const fullConfig: CLIConfig = { ...DEFAULT_READLINE_CONFIG, ...config }

  const validationResult = validateReadlineConfig(fullConfig)

  return match(
    () => createReadlineCLI(config),
    (error) => Err(error),
    validationResult
  )
}

/**
 * Create readline CLI with async initialization
 */
export async function createReadlineCLIAsync(
  config: Partial<CLIConfig> = {},
  options: {
    commandHandler?: ICommandHandler
    messageQueue?: QiAsyncMessageQueue<QiMessage>
  } = {}
): Promise<Result<ICLIFramework, QiError>> {
  // Extract messageQueue from config if not provided in options
  const messageQueue = options.messageQueue || (config as any).messageQueue
  const mergedOptions = {
    ...options,
    messageQueue,
  }

  const cliResult = createReadlineCLI(config, mergedOptions)

  return await match(
    async (cli): Promise<Result<ICLIFramework, QiError>> => {
      try {
        await cli.initialize()
        return Ok(cli)
      } catch (error) {
        return Err(
          factoryError(
            'CLI_INITIALIZATION_FAILED',
            `Failed to initialize CLI: ${error instanceof Error ? error.message : 'Unknown error'}`,
            { operation: 'initialize', framework: 'readline' }
          )
        )
      }
    },
    async (error: QiError): Promise<Result<ICLIFramework, QiError>> => Err(error),
    cliResult
  )
}

/**
 * Get default readline CLI configuration
 */
export function getDefaultReadlineConfig(): CLIConfig {
  return { ...DEFAULT_READLINE_CONFIG }
}

/**
 * Check if current environment supports readline features
 */
export function checkReadlineSupport(): {
  terminal: boolean
  colors: boolean
  unicode: boolean
  hotkeys: boolean
} {
  // More robust TTY detection - handle undefined values
  const hasStdoutTTY = process.stdout.isTTY === true
  const hasStdinTTY = process.stdin.isTTY === true

  // For development and testing, allow TTY functionality even if detection is unclear
  // This is safe because readline will gracefully handle non-TTY environments
  const isInteractiveEnvironment =
    hasStdoutTTY ||
    hasStdinTTY ||
    process.env.TERM !== undefined ||
    process.env.QI_FORCE_TTY === 'true'

  return {
    terminal: isInteractiveEnvironment,
    colors:
      !!(process.env.FORCE_COLOR && process.env.FORCE_COLOR !== '0') ||
      (hasStdoutTTY && !process.env.NO_COLOR && !process.env.NODE_DISABLE_COLORS) ||
      process.env.QI_FORCE_COLOR === 'true',
    unicode: process.platform !== 'win32',
    hotkeys: isInteractiveEnvironment,
  }
}
