/**
 * Message Type System for v-0.6.x Async Messaging
 *
 * Defines core message types and enums for the async message queue system
 * following Claude Code's message patterns with QiCore integration.
 */

import type { QiError } from '@qi/base'

/**
 * Core message types supported by the system
 */
export enum MessageType {
  // Command execution messages
  COMMAND = 'command',
  COMMAND_RESPONSE = 'command_response',
  COMMAND_ERROR = 'command_error',

  // User interaction messages
  USER_INPUT = 'user_input',
  USER_INTERRUPT = 'user_interrupt',

  // Agent output messages
  AGENT_OUTPUT = 'agent_output',
  AGENT_THINKING = 'agent_thinking',
  AGENT_ERROR = 'agent_error',

  // System control messages
  SYSTEM_CONTROL = 'system_control',
  SYSTEM_STATUS = 'system_status',
  SYSTEM_ABORT = 'system_abort',

  // Stream control messages
  STREAM_START = 'stream_start',
  STREAM_DATA = 'stream_data',
  STREAM_END = 'stream_end',
  STREAM_ERROR = 'stream_error',
}

/**
 * Message priority levels for queue ordering
 */
export enum MessagePriority {
  CRITICAL = 0, // System control, abort signals
  HIGH = 1, // User interrupts, errors
  NORMAL = 2, // Regular commands and responses
  LOW = 3, // Background processing, logging
}

/**
 * Message processing status
 */
export enum MessageStatus {
  PENDING = 'pending', // Queued for processing
  PROCESSING = 'processing', // Currently being processed
  COMPLETED = 'completed', // Successfully processed
  FAILED = 'failed', // Processing failed
  CANCELLED = 'cancelled', // Processing was cancelled
}

/**
 * Base message interface - all messages extend this
 */
export interface BaseMessage {
  readonly id: string
  readonly type: MessageType
  readonly timestamp: Date
  readonly priority: MessagePriority
  readonly correlationId?: string // For request/response correlation
  readonly parentId?: string // For message threading
}

/**
 * Command execution message
 */
export interface CommandMessage extends BaseMessage {
  type: MessageType.COMMAND
  command: string
  args: readonly unknown[]
  context: CommandContext
}

/**
 * Command response message
 */
export interface CommandResponseMessage extends BaseMessage {
  type: MessageType.COMMAND_RESPONSE
  result: unknown
  executionTime: number
  context: CommandContext
}

/**
 * Command error message
 */
export interface CommandErrorMessage extends BaseMessage {
  type: MessageType.COMMAND_ERROR
  error: QiError
  context: CommandContext
}

/**
 * User input message
 */
export interface UserInputMessage extends BaseMessage {
  type: MessageType.USER_INPUT
  input: string
  raw: boolean // true if raw input, false if processed
  source: 'stdin' | 'cli' | 'api'
}

/**
 * User interrupt message (Ctrl+C, etc.)
 */
export interface UserInterruptMessage extends BaseMessage {
  type: MessageType.USER_INTERRUPT
  signal: string // 'SIGINT', 'SIGTERM', etc.
  graceful: boolean // true if graceful shutdown requested
}

/**
 * Agent output message
 */
export interface AgentOutputMessage extends BaseMessage {
  type: MessageType.AGENT_OUTPUT
  content: string
  format: 'text' | 'markdown' | 'json'
  streaming: boolean // true if part of streaming response
}

/**
 * Agent thinking message (internal reasoning)
 */
export interface AgentThinkingMessage extends BaseMessage {
  type: MessageType.AGENT_THINKING
  thought: string
  confidence: number // 0-1 confidence in the reasoning
  visible: boolean // whether to show to user
}

/**
 * Agent error message
 */
export interface AgentErrorMessage extends BaseMessage {
  type: MessageType.AGENT_ERROR
  error: QiError
  recoverable: boolean
  suggestions?: string[]
}

/**
 * System control message
 */
export interface SystemControlMessage extends BaseMessage {
  type: MessageType.SYSTEM_CONTROL
  action: 'pause' | 'resume' | 'reset' | 'shutdown'
  reason?: string
  immediate: boolean // true if action should be immediate
}

/**
 * System status message
 */
export interface SystemStatusMessage extends BaseMessage {
  type: MessageType.SYSTEM_STATUS
  status: 'idle' | 'processing' | 'paused' | 'error'
  details: {
    queueSize: number
    activeProcesses: number
    uptime: number
    memoryUsage: number
  }
}

/**
 * System abort message
 */
export interface SystemAbortMessage extends BaseMessage {
  type: MessageType.SYSTEM_ABORT
  reason: string
  abortController?: AbortController
  cleanup: boolean // whether to run cleanup procedures
}

/**
 * Stream control messages
 */
export interface StreamStartMessage extends BaseMessage {
  type: MessageType.STREAM_START
  streamId: string
  expectedSize?: number
  metadata?: Record<string, unknown>
}

export interface StreamDataMessage extends BaseMessage {
  type: MessageType.STREAM_DATA
  streamId: string
  data: unknown
  sequence: number
  isLast: boolean
}

export interface StreamEndMessage extends BaseMessage {
  type: MessageType.STREAM_END
  streamId: string
  totalMessages: number
  success: boolean
}

export interface StreamErrorMessage extends BaseMessage {
  type: MessageType.STREAM_ERROR
  streamId: string
  error: QiError
  sequence?: number
}

/**
 * Union type of all possible messages
 */
export type QiMessage =
  | CommandMessage
  | CommandResponseMessage
  | CommandErrorMessage
  | UserInputMessage
  | UserInterruptMessage
  | AgentOutputMessage
  | AgentThinkingMessage
  | AgentErrorMessage
  | SystemControlMessage
  | SystemStatusMessage
  | SystemAbortMessage
  | StreamStartMessage
  | StreamDataMessage
  | StreamEndMessage
  | StreamErrorMessage

/**
 * Command context for execution tracking
 */
export interface CommandContext {
  readonly executionId: string
  readonly userId?: string
  readonly sessionId?: string
  readonly workingDirectory?: string
  readonly environment?: Record<string, string>
  readonly timeout?: number
  readonly retryCount?: number
  readonly abortController?: AbortController
}

/**
 * Message validation result
 */
export interface MessageValidationResult {
  valid: boolean
  errors: string[]
  warnings: string[]
}

/**
 * Message statistics for monitoring
 */
export interface MessageStats {
  totalMessages: number
  messagesByType: Record<MessageType, number>
  messagesByPriority: Record<MessagePriority, number>
  messagesByStatus: Record<MessageStatus, number>
  averageProcessingTime: number
  errorRate: number
  queueLength: number
}
