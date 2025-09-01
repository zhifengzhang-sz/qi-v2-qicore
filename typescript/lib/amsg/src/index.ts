/**
 * Messaging Module for v-0.6.x Async Messaging
 *
 * Core async message queue system inspired by Claude Code's h2A pattern,
 * adapted for QiCore functional programming with Result<T> error handling.
 */

// Implementations
export * from './impl'
export { QiAsyncMessageQueue } from './impl/QiAsyncMessageQueue'
export { QiMessageFactory } from './impl/QiMessageFactory'
// Interfaces
export * from './interfaces'

export {
  type IAsyncMessageQueue,
  QueueEventType,
  type QueueOptions,
  type QueueState,
} from './interfaces/IAsyncMessageQueue'
// Types and enums
export * from './types'
// Convenience re-exports for common usage
export {
  type AgentOutputMessage,
  type BaseMessage,
  type CommandMessage,
  MessagePriority,
  MessageStatus,
  MessageType,
  type QiMessage,
  type SystemControlMessage,
  type UserInputMessage,
} from './types/MessageTypes'
