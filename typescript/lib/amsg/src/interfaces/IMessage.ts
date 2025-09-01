/**
 * Core Message Interface for v-0.6.x Async Messaging
 *
 * Defines the fundamental message interface and validation contracts
 * following QiCore patterns with Result<T> error handling.
 */

import type {
  BaseMessage,
  MessageStats,
  MessageValidationResult,
  QiMessage,
} from '../types/MessageTypes'
import type { QiError, Result } from '@qi/base'

/**
 * Message factory interface for creating typed messages
 */
export interface IMessageFactory {
  /**
   * Create a message with automatic ID generation and timestamp
   */
  createMessage<T extends QiMessage>(
    type: T['type'],
    payload: Omit<T, keyof BaseMessage>
  ): Result<T, QiError>

  /**
   * Create a command message
   */
  createCommandMessage(
    command: string,
    args: readonly unknown[],
    context: unknown
  ): Result<QiMessage, QiError>

  /**
   * Create a user input message
   */
  createUserInputMessage(
    input: string,
    source: 'stdin' | 'cli' | 'api',
    raw?: boolean
  ): Result<QiMessage, QiError>

  /**
   * Create an agent output message
   */
  createAgentOutputMessage(
    content: string,
    format?: 'text' | 'markdown' | 'json',
    streaming?: boolean
  ): Result<QiMessage, QiError>

  /**
   * Create a system control message
   */
  createSystemControlMessage(
    action: 'pause' | 'resume' | 'reset' | 'shutdown',
    immediate?: boolean,
    reason?: string
  ): Result<QiMessage, QiError>
}

/**
 * Message validator interface for type and content validation
 */
export interface IMessageValidator {
  /**
   * Validate message structure and content
   */
  validate(message: QiMessage): Result<MessageValidationResult, QiError>

  /**
   * Validate message type compatibility
   */
  validateType(message: QiMessage, expectedType: QiMessage['type']): Result<boolean, QiError>

  /**
   * Validate message context
   */
  validateContext(message: QiMessage): Result<boolean, QiError>

  /**
   * Check if message is expired based on timestamp and TTL
   */
  isExpired(message: QiMessage, ttlMs?: number): Result<boolean, QiError>
}

/**
 * Message serializer interface for persistence and transport
 */
export interface IMessageSerializer {
  /**
   * Serialize message to JSON string
   */
  serialize(message: QiMessage): Result<string, QiError>

  /**
   * Deserialize JSON string to message
   */
  deserialize(json: string): Result<QiMessage, QiError>

  /**
   * Serialize message to binary format for efficient transport
   */
  serializeBinary(message: QiMessage): Result<Uint8Array, QiError>

  /**
   * Deserialize binary format to message
   */
  deserializeBinary(data: Uint8Array): Result<QiMessage, QiError>
}

/**
 * Message transformer interface for message processing pipelines
 */
export interface IMessageTransformer<TInput extends QiMessage, TOutput extends QiMessage> {
  /**
   * Transform input message to output message
   */
  transform(input: TInput): Result<TOutput, QiError>

  /**
   * Check if transformer can handle input message type
   */
  canTransform(message: QiMessage): boolean

  /**
   * Get transformer metadata
   */
  getMetadata(): {
    inputType: TInput['type']
    outputType: TOutput['type']
    name: string
    version: string
  }
}

/**
 * Message interceptor interface for middleware-style processing
 */
export interface IMessageInterceptor {
  /**
   * Intercept message before processing
   */
  beforeProcess(message: QiMessage): Result<QiMessage, QiError>

  /**
   * Intercept message after processing
   */
  afterProcess(message: QiMessage, result: unknown): Result<QiMessage, QiError>

  /**
   * Handle processing errors
   */
  onError(message: QiMessage, error: QiError): Result<QiMessage, QiError>

  /**
   * Get interceptor priority (lower numbers = higher priority)
   */
  getPriority(): number
}

/**
 * Message handler interface for processing specific message types
 */
export interface IMessageHandler<T extends QiMessage = QiMessage> {
  /**
   * Handle message processing
   */
  handle(message: T): Promise<Result<unknown, QiError>>

  /**
   * Check if handler can process message type
   */
  canHandle(message: QiMessage): message is T

  /**
   * Get handler metadata
   */
  getMetadata(): {
    messageType: T['type']
    name: string
    priority: number
  }
}

/**
 * Message router interface for directing messages to appropriate handlers
 */
export interface IMessageRouter {
  /**
   * Register message handler
   */
  registerHandler<T extends QiMessage>(handler: IMessageHandler<T>): Result<void, QiError>

  /**
   * Unregister message handler
   */
  unregisterHandler<T extends QiMessage>(handler: IMessageHandler<T>): Result<void, QiError>

  /**
   * Route message to appropriate handler
   */
  route(message: QiMessage): Promise<Result<unknown, QiError>>

  /**
   * Get all registered handlers
   */
  getHandlers(): readonly IMessageHandler[]

  /**
   * Get handlers for specific message type
   */
  getHandlersForType(messageType: QiMessage['type']): readonly IMessageHandler[]
}

/**
 * Message statistics tracker interface
 */
export interface IMessageStatsTracker {
  /**
   * Record message processing start
   */
  recordProcessingStart(message: QiMessage): Result<void, QiError>

  /**
   * Record message processing completion
   */
  recordProcessingComplete(message: QiMessage, duration: number): Result<void, QiError>

  /**
   * Record message processing error
   */
  recordProcessingError(message: QiMessage, error: QiError): Result<void, QiError>

  /**
   * Get current statistics
   */
  getStats(): Result<MessageStats, QiError>

  /**
   * Reset statistics
   */
  resetStats(): Result<void, QiError>

  /**
   * Get statistics for specific time period
   */
  getStatsForPeriod(startTime: Date, endTime: Date): Result<MessageStats, QiError>
}
