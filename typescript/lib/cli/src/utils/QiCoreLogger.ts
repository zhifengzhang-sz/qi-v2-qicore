/**
 * QiCore Logger Utility
 *
 * Provides a standardized logger factory with graceful fallback
 * for all qi-prompt components.
 */

import { match, type QiError } from '@qi/base'
import { type Logger, createLogger as qiCreateLogger } from '@qi/core'

/**
 * Standard metadata interface for all log entries
 */
export interface LogMetadata {
  component: string // Required: component name
  method?: string // Optional: method name
  step?: string // Optional: processing step
  messageId?: string // Optional: message ID for message processing
  errorMessage?: string // Optional: error message
  errorContext?: any // Optional: additional error context
  errorStack?: string // Optional: error stack trace
  performance?: {
    // Optional: performance metrics
    duration?: number
    memory?: number
    startTime?: number
    endTime?: number
  }
  trace?: string // Optional: trace identifier
  requestId?: string // Optional: request correlation ID
  userId?: string // Optional: user identifier
  sessionId?: string // Optional: session identifier
  operationType?: string // Optional: type of operation being performed
  version?: string // Optional: component or system version
  environment?: string // Optional: environment (dev, staging, prod)
  [key: string]: any // Additional metadata
}

/**
 * Metadata builder for creating standardized metadata objects
 */
export class MetadataBuilder {
  private metadata: LogMetadata

  constructor(component: string) {
    this.metadata = { component }
  }

  method(method: string): MetadataBuilder {
    this.metadata.method = method
    return this
  }

  step(step: string): MetadataBuilder {
    this.metadata.step = step
    return this
  }

  messageId(messageId: string): MetadataBuilder {
    this.metadata.messageId = messageId
    return this
  }

  requestId(requestId: string): MetadataBuilder {
    this.metadata.requestId = requestId
    return this
  }

  operationType(operationType: string): MetadataBuilder {
    this.metadata.operationType = operationType
    return this
  }

  performance(performance: LogMetadata['performance']): MetadataBuilder {
    this.metadata.performance = performance
    return this
  }

  error(error: Error | any): MetadataBuilder {
    this.metadata.errorMessage = error instanceof Error ? error.message : String(error)
    this.metadata.errorStack = error instanceof Error ? error.stack : undefined
    this.metadata.errorContext =
      error && typeof error === 'object' && 'context' in error
        ? (error as { context: unknown }).context
        : undefined
    return this
  }

  custom(key: string, value: any): MetadataBuilder {
    this.metadata[key] = value
    return this
  }

  build(): LogMetadata {
    return { ...this.metadata }
  }
}

/**
 * Create a metadata builder for a component
 */
export const createMetadata = (component: string): MetadataBuilder => {
  return new MetadataBuilder(component)
}

/**
 * Simple logger interface for fallback when QiCore is unavailable
 * Matches existing qi-prompt logger interface: (message, data, metadata)
 */
export interface SimpleLogger {
  info: (message: string, data?: any, metadata?: any) => void
  error: (message: string, data?: any, metadata?: any) => void
  warn: (message: string, data?: any, metadata?: any) => void
  debug: (message: string, data?: any, metadata?: any) => void
}

/**
 * Logger configuration options
 */
export interface LoggerConfig {
  level?: string // Log level (debug, info, warn, error)
  pretty?: boolean // Pretty print logs
  name?: string // Logger name/component
  enablePerformanceLogging?: boolean // Enable automatic performance logging
}

/**
 * Create a simple fallback logger when QiCore logger is not available
 */
const createFallbackLogger = (config: LoggerConfig): SimpleLogger => {
  const levels = { debug: 0, info: 1, warn: 2, error: 3 }
  const currentLevel = levels[config.level as keyof typeof levels] || 1
  const componentName = config.name || 'unknown'

  const log = (logLevel: string, message: string, _data?: any, metadata?: any) => {
    if (levels[logLevel as keyof typeof levels] >= currentLevel) {
      const timestamp = new Date().toISOString()
      const component = metadata?.component || componentName

      if (config.pretty && metadata) {
        console.log(`[${timestamp}] ${logLevel.toUpperCase()} [${component}]: ${message}`, metadata)
      } else {
        console.log(`[${timestamp}] ${logLevel.toUpperCase()} [${component}]: ${message}`)
      }
    }
  }

  return {
    info: (msg, data, meta) => log('info', msg, data, meta),
    error: (msg, data, meta) => log('error', msg, data, meta),
    warn: (msg, data, meta) => log('warn', msg, data, meta),
    debug: (msg, data, meta) => log('debug', msg, data, meta),
  }
}

/**
 * Create a logger instance using QiCore with graceful fallback
 * Adapts QiCore logger (2 params) to SimpleLogger interface (3 params)
 */
export const createQiLogger = (config: LoggerConfig = {}): SimpleLogger => {
  const loggerConfig = {
    level: config.level || 'info',
    pretty: config.pretty !== false, // Default to true
    name: config.name || 'qi-prompt',
    ...config,
  }

  try {
    // Attempt to create QiCore logger
    const qiLoggerResult = qiCreateLogger({
      level: loggerConfig.level as 'debug' | 'info' | 'warn' | 'error',
      pretty: loggerConfig.pretty,
      name: loggerConfig.name,
    })

    return match(
      (qiLogger: Logger) => {
        // Adapt QiCore logger (2 params) to SimpleLogger interface (3 params)
        return {
          info: (message: string, data?: any, metadata?: any) => {
            qiLogger.info(message, metadata || data)
          },
          error: (message: string, data?: any, metadata?: any) => {
            qiLogger.error(message, metadata || data)
          },
          warn: (message: string, data?: any, metadata?: any) => {
            qiLogger.warn(message, metadata || data)
          },
          debug: (message: string, data?: any, metadata?: any) => {
            qiLogger.debug(message, metadata || data)
          },
        }
      },
      () => {
        // Fallback to simple logger
        return createFallbackLogger(loggerConfig)
      },
      qiLoggerResult
    )
  } catch (_error) {
    // If QiCore is not available, use fallback
    return createFallbackLogger(loggerConfig)
  }
}

/**
 * Create a conditional debug logger that only logs when debug mode is enabled
 */
export const createConditionalLogger = (
  config: LoggerConfig & { debugMode?: boolean } = {}
): SimpleLogger => {
  const logger = createQiLogger({
    ...config,
    level: config.debugMode ? 'debug' : config.level || 'info',
  })

  // If debug mode is disabled, wrap logger to make debug calls no-op
  if (!config.debugMode) {
    return {
      info: logger.info.bind(logger),
      warn: logger.warn.bind(logger),
      error: logger.error.bind(logger),
      debug: () => {}, // No-op for debug when debug mode is disabled
    }
  }

  return logger
}

/**
 * Performance logging wrapper
 */
export class PerformanceLogger {
  private logger: SimpleLogger
  private timers = new Map<string, number>()

  constructor(logger: SimpleLogger) {
    this.logger = logger
  }

  /**
   * Start a performance timer
   */
  startTimer(timerId: string, metadata?: LogMetadata): void {
    this.timers.set(timerId, Date.now())

    this.logger.debug(`⏱️ Started timer: ${timerId}`, undefined, {
      ...metadata,
      trace: 'performance_start',
      timerId,
    })
  }

  /**
   * End a performance timer and log the result
   */
  endTimer(timerId: string, message: string, metadata?: LogMetadata): void {
    const startTime = this.timers.get(timerId)
    if (!startTime) {
      this.logger.warn(`⏱️ Timer not found: ${timerId}`, undefined, {
        ...metadata,
        timerId,
      })
      return
    }

    const duration = Date.now() - startTime
    this.timers.delete(timerId)

    this.logger.info(`⏱️ ${message}`, undefined, {
      ...metadata,
      trace: 'performance_end',
      timerId,
      performance: {
        duration,
        startTime,
        endTime: Date.now(),
        ...metadata?.performance,
      },
    })
  }

  /**
   * Log performance metrics
   */
  logPerformance(operation: string, duration: number, metadata?: LogMetadata): void {
    this.logger.info(`📊 Performance: ${operation}`, undefined, {
      ...metadata,
      trace: 'performance_metric',
      operation,
      performance: {
        duration,
        ...metadata?.performance,
      },
    })
  }
}

/**
 * Create a performance-enabled logger
 */
export const createPerformanceLogger = (
  config: LoggerConfig = {}
): { logger: SimpleLogger; performance: PerformanceLogger } => {
  const logger = createQiLogger(config)
  const performance = new PerformanceLogger(logger)

  return { logger, performance }
}

/**
 * Standard error logging helper
 */
export const logError = (
  logger: SimpleLogger,
  error: Error | QiError | any,
  context: LogMetadata
): void => {
  const errorMessage = error instanceof Error ? error.message : String(error)
  const errorStack = error instanceof Error ? error.stack : undefined

  logger.error('❌ Error occurred', undefined, {
    ...context,
    errorMessage,
    errorStack,
    errorContext:
      (error && typeof error === 'object' && 'context' in error
        ? (error as { context: unknown }).context
        : undefined) || context.errorContext,
    trace: 'error',
  })
}

/**
 * Standard success logging helper
 */
export const logSuccess = (logger: SimpleLogger, message: string, context: LogMetadata): void => {
  logger.info(`✅ ${message}`, undefined, {
    ...context,
    trace: 'success',
  })
}

/**
 * Standard warning logging helper
 */
export const logWarning = (logger: SimpleLogger, message: string, context: LogMetadata): void => {
  logger.warn(`⚠️ ${message}`, undefined, {
    ...context,
    trace: 'warning',
  })
}

/**
 * Standard debug logging helper (respects debug mode)
 */
export const logDebug = (
  logger: SimpleLogger,
  message: string,
  context: LogMetadata & { debugMode?: boolean }
): void => {
  // Only log debug messages if debug mode is enabled
  if (context.debugMode !== false) {
    logger.debug(`🔍 ${message}`, undefined, {
      ...context,
      debugMode: undefined, // Remove debugMode from logged metadata
      trace: 'debug',
    })
  }
}

/**
 * Component-specific logging helpers
 */
export const ComponentLogging = {
  /**
   * Agent logging helpers
   */
  Agent: {
    requestReceived: (logger: SimpleLogger, requestId: string, metadata?: LogMetadata) => {
      logger.info('🔄 Agent request received', undefined, {
        component: 'Agent',
        method: 'handleRequest',
        requestId,
        operationType: 'request_received',
        trace: 'agent_request',
        ...metadata,
      })
    },

    requestCompleted: (
      logger: SimpleLogger,
      requestId: string,
      duration: number,
      metadata?: LogMetadata
    ) => {
      logger.info('✅ Agent request completed', undefined, {
        component: 'Agent',
        method: 'handleRequest',
        requestId,
        operationType: 'request_completed',
        performance: { duration },
        trace: 'agent_response',
        ...metadata,
      })
    },

    configurationUpdated: (logger: SimpleLogger, configKey: string, metadata?: LogMetadata) => {
      logger.info('⚙️ Agent configuration updated', undefined, {
        component: 'Agent',
        method: 'updateConfiguration',
        operationType: 'config_update',
        configKey,
        trace: 'agent_config',
        ...metadata,
      })
    },
  },

  /**
   * State management logging helpers
   */
  State: {
    stateChanged: (
      logger: SimpleLogger,
      field: string,
      oldValue: any,
      newValue: any,
      metadata?: LogMetadata
    ) => {
      logger.debug('🔄 State changed', undefined, {
        component: 'StateManager',
        method: 'updateState',
        operationType: 'state_change',
        field,
        oldValue,
        newValue,
        trace: 'state_update',
        ...metadata,
      })
    },

    stateLoaded: (logger: SimpleLogger, source: string, metadata?: LogMetadata) => {
      logger.info('📂 State loaded', undefined, {
        component: 'StateManager',
        method: 'loadState',
        operationType: 'state_load',
        source,
        trace: 'state_init',
        ...metadata,
      })
    },

    statePersisted: (logger: SimpleLogger, target: string, metadata?: LogMetadata) => {
      logger.info('💾 State persisted', undefined, {
        component: 'StateManager',
        method: 'persistState',
        operationType: 'state_persist',
        target,
        trace: 'state_save',
        ...metadata,
      })
    },
  },

  /**
   * CLI framework logging helpers
   */
  CLI: {
    frameworkInitialized: (logger: SimpleLogger, frameworkType: string, metadata?: LogMetadata) => {
      logger.info('🚀 CLI Framework initialized', undefined, {
        component: 'CLIFramework',
        method: 'initialize',
        operationType: 'framework_init',
        frameworkType,
        trace: 'cli_init',
        ...metadata,
      })
    },

    inputReceived: (logger: SimpleLogger, inputLength: number, metadata?: LogMetadata) => {
      logger.debug('⌨️ User input received', undefined, {
        component: 'CLIFramework',
        method: 'handleInput',
        operationType: 'user_input',
        inputLength,
        trace: 'cli_input',
        ...metadata,
      })
    },

    commandExecuted: (
      logger: SimpleLogger,
      command: string,
      duration: number,
      metadata?: LogMetadata
    ) => {
      logger.info('⚡ Command executed', undefined, {
        component: 'CLIFramework',
        method: 'executeCommand',
        operationType: 'command_execution',
        command,
        performance: { duration },
        trace: 'cli_command',
        ...metadata,
      })
    },
  },

  /**
   * Prompt processing logging helpers
   */
  Prompt: {
    templateLoaded: (
      logger: SimpleLogger,
      templateType: string,
      templateSource: string,
      metadata?: LogMetadata
    ) => {
      logger.debug('📄 Template loaded', undefined, {
        component: 'PromptHandler',
        method: 'loadTemplate',
        operationType: 'template_load',
        templateType,
        templateSource,
        trace: 'prompt_template',
        ...metadata,
      })
    },

    contextProcessed: (
      logger: SimpleLogger,
      contextSize: number,
      duration: number,
      metadata?: LogMetadata
    ) => {
      logger.debug('🧠 Context processed', undefined, {
        component: 'PromptHandler',
        method: 'processContext',
        operationType: 'context_processing',
        contextSize,
        performance: { duration },
        trace: 'prompt_context',
        ...metadata,
      })
    },

    llmRequestSent: (
      logger: SimpleLogger,
      provider: string,
      model: string,
      promptLength: number,
      metadata?: LogMetadata
    ) => {
      logger.info('🤖 LLM request sent', undefined, {
        component: 'PromptHandler',
        method: 'sendLLMRequest',
        operationType: 'llm_request',
        provider,
        model,
        promptLength,
        trace: 'llm_interaction',
        ...metadata,
      })
    },

    llmResponseReceived: (
      logger: SimpleLogger,
      provider: string,
      model: string,
      responseLength: number,
      duration: number,
      metadata?: LogMetadata
    ) => {
      logger.info('📝 LLM response received', undefined, {
        component: 'PromptHandler',
        method: 'receiveLLMResponse',
        operationType: 'llm_response',
        provider,
        model,
        responseLength,
        performance: { duration },
        trace: 'llm_interaction',
        ...metadata,
      })
    },
  },
}

/**
 * Message processing logging helpers
 */
export const MessageLogging = {
  messageCreated: (
    logger: SimpleLogger,
    messageId: string,
    messageType: string,
    metadata?: LogMetadata
  ) => {
    logger.debug('📤 Message created', undefined, {
      component: 'MessageFactory',
      ...metadata,
      messageId,
      messageType,
      operationType: 'message_creation',
      trace: 'message_lifecycle',
    })
  },

  messageEnqueued: (
    logger: SimpleLogger,
    messageId: string,
    messageType: string,
    queueSize?: number,
    metadata?: LogMetadata
  ) => {
    logger.debug('📥 Message enqueued', undefined, {
      component: 'MessageQueue',
      ...metadata,
      messageId,
      messageType,
      queueSize,
      operationType: 'message_enqueue',
      trace: 'message_lifecycle',
    })
  },

  messageProcessingStarted: (
    logger: SimpleLogger,
    messageId: string,
    messageType: string,
    metadata?: LogMetadata
  ) => {
    logger.debug('⏳ Message processing started', undefined, {
      component: 'MessageProcessor',
      ...metadata,
      messageId,
      messageType,
      operationType: 'message_processing_start',
      trace: 'message_lifecycle',
    })
  },

  messageProcessingCompleted: (
    logger: SimpleLogger,
    messageId: string,
    messageType: string,
    duration?: number,
    metadata?: LogMetadata
  ) => {
    logger.debug('✅ Message processing completed', undefined, {
      component: 'MessageProcessor',
      ...metadata,
      messageId,
      messageType,
      performance: duration ? { duration } : undefined,
      operationType: 'message_processing_complete',
      trace: 'message_lifecycle',
    })
  },

  messageProcessingFailed: (
    logger: SimpleLogger,
    messageId: string,
    messageType: string,
    error: any,
    metadata?: LogMetadata
  ) => {
    logError(logger, error, {
      component: 'MessageProcessor',
      ...metadata,
      messageId,
      messageType,
      operationType: 'message_processing_error',
      trace: 'message_lifecycle',
    })
  },
}

/**
 * Predefined trace categories for consistent logging
 */
export const TraceCategories = {
  STARTUP: 'startup',
  SHUTDOWN: 'shutdown',
  REQUEST: 'request',
  RESPONSE: 'response',
  ERROR: 'error',
  WARNING: 'warning',
  DEBUG: 'debug',
  PERFORMANCE: 'performance',
  STATE_CHANGE: 'state_change',
  CONFIG_CHANGE: 'config_change',
  MESSAGE_LIFECYCLE: 'message_lifecycle',
  LLM_INTERACTION: 'llm_interaction',
  CLI_INTERACTION: 'cli_interaction',
  USER_ACTION: 'user_action',
  SYSTEM_EVENT: 'system_event',
} as const

/**
 * Common operation types for consistent metadata
 */
export const OperationTypes = {
  // Agent operations
  AGENT_REQUEST: 'agent_request',
  AGENT_RESPONSE: 'agent_response',
  AGENT_CONFIG: 'agent_config',

  // State operations
  STATE_LOAD: 'state_load',
  STATE_SAVE: 'state_save',
  STATE_UPDATE: 'state_update',

  // CLI operations
  CLI_INIT: 'cli_init',
  CLI_INPUT: 'cli_input',
  CLI_COMMAND: 'cli_command',

  // Prompt operations
  PROMPT_TEMPLATE: 'prompt_template',
  PROMPT_CONTEXT: 'prompt_context',
  LLM_REQUEST: 'llm_request',
  LLM_RESPONSE: 'llm_response',

  // Message operations
  MESSAGE_CREATE: 'message_create',
  MESSAGE_ENQUEUE: 'message_enqueue',
  MESSAGE_PROCESS: 'message_process',

  // System operations
  SYSTEM_INIT: 'system_init',
  SYSTEM_SHUTDOWN: 'system_shutdown',
  HEALTH_CHECK: 'health_check',
} as const

// Export types for use in other modules
export type { Logger }
