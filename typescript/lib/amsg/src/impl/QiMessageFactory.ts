/**
 * QiMessageFactory Implementation
 *
 * Factory for creating typed messages with automatic ID generation and validation.
 * Integrates with QiCore Result<T> patterns for error handling.
 */

import { randomUUID } from 'node:crypto'
import type { IMessageFactory } from '../interfaces/IMessage'
import type {
  AgentOutputMessage,
  BaseMessage,
  CommandContext,
  CommandMessage,
  QiMessage,
  SystemControlMessage,
  UserInputMessage,
} from '../types/MessageTypes'
import { MessagePriority, MessageType } from '../types/MessageTypes'
import { create, failure, match, type QiError, type Result, success } from '@qi/base'

/**
 * Factory error types
 */
interface FactoryError extends QiError {
  context: {
    operation?: string
    messageType?: string
    validation?: string[]
  }
}

const factoryError = (
  code: string,
  message: string,
  context: FactoryError['context'] = {}
): FactoryError => create(code, message, 'VALIDATION', context) as FactoryError

/**
 * QiMessageFactory implementation
 */
export class QiMessageFactory implements IMessageFactory {
  /**
   * Create a message with automatic ID generation and timestamp
   */
  createMessage<T extends QiMessage>(
    type: T['type'],
    payload: Omit<T, keyof BaseMessage>
  ): Result<T, FactoryError> {
    // Validate message type
    if (!Object.values(MessageType).includes(type as MessageType)) {
      return failure(
        factoryError('INVALID_MESSAGE_TYPE', `Unknown message type: ${type}`, {
          operation: 'createMessage',
          messageType: type,
        })
      )
    }

    // Create base message properties
    const baseMessage: BaseMessage = {
      id: randomUUID(),
      type: type as MessageType,
      timestamp: new Date(),
      priority: this.inferPriority(type),
    }

    // Add optional properties if they exist in payload
    const payloadAny = payload as Record<string, unknown>
    if (payloadAny.correlationId) {
      ;(baseMessage as unknown as Record<string, unknown>).correlationId = payloadAny.correlationId
    }
    if (payloadAny.parentId) {
      ;(baseMessage as unknown as Record<string, unknown>).parentId = payloadAny.parentId
    }

    // Combine base with payload
    const message = {
      ...baseMessage,
      ...payload,
    } as T

    // Validate created message
    const validationResult = this.validateMessage(message)
    return match(
      () => success(message),
      (error: FactoryError) => failure(error) as Result<T, FactoryError>,
      validationResult
    )
  }

  /**
   * Create a command message
   */
  createCommandMessage(
    command: string,
    args: readonly unknown[],
    context: CommandContext
  ): Result<CommandMessage, FactoryError> {
    // Validate inputs
    if (!command || typeof command !== 'string') {
      return failure(
        factoryError('INVALID_COMMAND', 'Command must be a non-empty string', {
          operation: 'createCommandMessage',
        })
      )
    }

    if (!Array.isArray(args)) {
      return failure(
        factoryError('INVALID_ARGS', 'Args must be an array', {
          operation: 'createCommandMessage',
        })
      )
    }

    if (!context || !context.executionId) {
      return failure(
        factoryError('INVALID_CONTEXT', 'Context must include executionId', {
          operation: 'createCommandMessage',
        })
      )
    }

    return this.createMessage<CommandMessage>(MessageType.COMMAND, {
      command,
      args,
      context,
    })
  }

  /**
   * Create a user input message
   */
  createUserInputMessage(
    input: string,
    source: 'stdin' | 'cli' | 'api',
    raw = false
  ): Result<UserInputMessage, FactoryError> {
    // Validate inputs
    if (typeof input !== 'string') {
      return failure(
        factoryError('INVALID_INPUT', 'Input must be a string', {
          operation: 'createUserInputMessage',
        })
      )
    }

    if (!['stdin', 'cli', 'api'].includes(source)) {
      return failure(
        factoryError('INVALID_SOURCE', 'Source must be stdin, cli, or api', {
          operation: 'createUserInputMessage',
        })
      )
    }

    return this.createMessage<UserInputMessage>(MessageType.USER_INPUT, {
      input,
      raw,
      source,
    })
  }

  /**
   * Create an agent output message
   */
  createAgentOutputMessage(
    content: string,
    format: 'text' | 'markdown' | 'json' = 'text',
    streaming = false
  ): Result<AgentOutputMessage, FactoryError> {
    // Validate inputs
    if (typeof content !== 'string') {
      return failure(
        factoryError('INVALID_CONTENT', 'Content must be a string', {
          operation: 'createAgentOutputMessage',
        })
      )
    }

    if (!['text', 'markdown', 'json'].includes(format)) {
      return failure(
        factoryError('INVALID_FORMAT', 'Format must be text, markdown, or json', {
          operation: 'createAgentOutputMessage',
        })
      )
    }

    // JSON format validation
    if (format === 'json') {
      try {
        JSON.parse(content)
      } catch (error) {
        return failure(
          factoryError('INVALID_JSON', 'Content is not valid JSON', {
            operation: 'createAgentOutputMessage',
            validation: [(error as Error).message],
          })
        )
      }
    }

    return this.createMessage<AgentOutputMessage>(MessageType.AGENT_OUTPUT, {
      content,
      format,
      streaming,
    })
  }

  /**
   * Create a system control message
   */
  createSystemControlMessage(
    action: 'pause' | 'resume' | 'reset' | 'shutdown',
    immediate = false,
    reason?: string
  ): Result<SystemControlMessage, FactoryError> {
    // Validate action
    if (!['pause', 'resume', 'reset', 'shutdown'].includes(action)) {
      return failure(
        factoryError('INVALID_ACTION', 'Invalid system control action', {
          operation: 'createSystemControlMessage',
        })
      )
    }

    return this.createMessage<SystemControlMessage>(MessageType.SYSTEM_CONTROL, {
      action,
      immediate,
      ...(reason && { reason }),
    })
  }

  // Private helper methods

  private inferPriority(type: MessageType): MessagePriority {
    // Map message types to default priorities
    const priorityMap: Record<MessageType, MessagePriority> = {
      // Critical priority
      [MessageType.SYSTEM_ABORT]: MessagePriority.CRITICAL,
      [MessageType.USER_INTERRUPT]: MessagePriority.CRITICAL,

      // High priority
      [MessageType.SYSTEM_CONTROL]: MessagePriority.HIGH,
      [MessageType.COMMAND_ERROR]: MessagePriority.HIGH,
      [MessageType.AGENT_ERROR]: MessagePriority.HIGH,
      [MessageType.STREAM_ERROR]: MessagePriority.HIGH,

      // Normal priority
      [MessageType.COMMAND]: MessagePriority.NORMAL,
      [MessageType.COMMAND_RESPONSE]: MessagePriority.NORMAL,
      [MessageType.USER_INPUT]: MessagePriority.NORMAL,
      [MessageType.AGENT_OUTPUT]: MessagePriority.NORMAL,
      [MessageType.STREAM_START]: MessagePriority.NORMAL,
      [MessageType.STREAM_DATA]: MessagePriority.NORMAL,
      [MessageType.STREAM_END]: MessagePriority.NORMAL,

      // Low priority
      [MessageType.AGENT_THINKING]: MessagePriority.LOW,
      [MessageType.SYSTEM_STATUS]: MessagePriority.LOW,
    }

    return priorityMap[type] ?? MessagePriority.NORMAL
  }

  private validateMessage<T extends QiMessage>(message: T): Result<T, FactoryError> {
    const errors: string[] = []

    // Basic validation
    if (!message.id) {
      errors.push('Message ID is required')
    }

    if (!message.type) {
      errors.push('Message type is required')
    }

    if (!message.timestamp) {
      errors.push('Message timestamp is required')
    }

    if (message.priority === undefined || message.priority === null) {
      errors.push('Message priority is required')
    }

    // Type-specific validation
    switch (message.type) {
      case MessageType.COMMAND: {
        const cmdMsg = message as CommandMessage
        if (!cmdMsg.command) {
          errors.push('Command is required for command messages')
        }
        if (!cmdMsg.args) {
          errors.push('Args are required for command messages')
        }
        if (!cmdMsg.context?.executionId) {
          errors.push('Execution context is required for command messages')
        }
        break
      }

      case MessageType.USER_INPUT: {
        const userMsg = message as UserInputMessage
        if (!userMsg.source) {
          errors.push('Source is required for user input messages')
        }
        break
      }

      case MessageType.AGENT_OUTPUT: {
        const agentMsg = message as AgentOutputMessage
        if (!agentMsg.content) {
          errors.push('Content is required for agent output messages')
        }
        if (!agentMsg.format) {
          errors.push('Format is required for agent output messages')
        }
        break
      }

      case MessageType.SYSTEM_CONTROL: {
        const sysMsg = message as SystemControlMessage
        if (!sysMsg.action) {
          errors.push('Action is required for system control messages')
        }
        break
      }
    }

    // Check for errors
    if (errors.length > 0) {
      return failure(
        factoryError('VALIDATION_FAILED', 'Message validation failed', {
          operation: 'validateMessage',
          messageType: message.type,
          validation: errors,
        })
      )
    }

    return success(message)
  }

  /**
   * Create a correlation ID for request/response patterns
   */
  static createCorrelationId(): string {
    return randomUUID()
  }

  /**
   * Create message with correlation to parent
   */
  createCorrelatedMessage<T extends QiMessage>(
    type: T['type'],
    payload: Omit<T, keyof BaseMessage>,
    parentMessage: QiMessage
  ): Result<T, FactoryError> {
    const correlatedPayload = {
      ...payload,
      correlationId: parentMessage.correlationId || parentMessage.id,
      parentId: parentMessage.id,
    }

    return this.createMessage<T>(type, correlatedPayload)
  }

  /**
   * Create response message for a command
   */
  createResponseMessage(
    commandMessage: CommandMessage,
    result: unknown,
    executionTime: number
  ): Result<QiMessage, FactoryError> {
    return this.createCorrelatedMessage(
      MessageType.COMMAND_RESPONSE,
      {
        result,
        executionTime,
        context: commandMessage.context,
      },
      commandMessage
    )
  }

  /**
   * Create error message for a command
   */
  createErrorMessage(
    commandMessage: CommandMessage,
    error: QiError
  ): Result<QiMessage, FactoryError> {
    return this.createCorrelatedMessage(
      MessageType.COMMAND_ERROR,
      {
        error,
        context: commandMessage.context,
      },
      commandMessage
    )
  }
}
