/**
 * Async Message Queue Interface for v-0.6.x
 *
 * Defines the contract for h2A-inspired async message queue with QiCore integration.
 * Provides non-blocking message processing with Promise-based flow control.
 */

import type { MessagePriority, MessageStats, QiMessage } from '../types/MessageTypes'
import type { QiError, Result } from '@qi/base'

/**
 * Queue state information
 */
export interface QueueState {
  readonly started: boolean
  readonly isDone: boolean
  readonly hasError: boolean
  readonly messageCount: number
  readonly processingCount: number
  readonly errorCount: number
}

/**
 * Queue configuration options
 */
export interface QueueOptions {
  /**
   * Maximum queue size (0 = unlimited)
   */
  maxSize?: number

  /**
   * Maximum concurrent processing
   */
  maxConcurrent?: number

  /**
   * Message TTL in milliseconds
   */
  messageTtl?: number

  /**
   * Priority queuing enabled
   */
  priorityQueuing?: boolean

  /**
   * Auto-cleanup expired messages
   */
  autoCleanup?: boolean

  /**
   * Statistics tracking enabled
   */
  enableStats?: boolean

  /**
   * Cleanup function called when queue is destroyed
   */
  cleanupFn?: () => void | Promise<void>
}

/**
 * Async message queue interface following h2A pattern
 */
export interface IAsyncMessageQueue<T extends QiMessage = QiMessage> extends AsyncIterable<T> {
  /**
   * Enqueue a message for processing
   * Returns immediately, supports real-time message injection
   */
  enqueue(message: T): Result<void, QiError>

  /**
   * Mark queue as complete (no more messages will be added)
   */
  done(): Result<void, QiError>

  /**
   * Signal error condition and reject waiting readers
   */
  error(error: QiError): Result<void, QiError>

  /**
   * Get current queue state
   */
  getState(): Result<QueueState, QiError>

  /**
   * Get queue statistics (if enabled)
   */
  getStats(): Result<MessageStats | null, QiError>

  /**
   * Peek at next message without removing it
   */
  peek(): Result<T | null, QiError>

  /**
   * Get queue size
   */
  size(): Result<number, QiError>

  /**
   * Check if queue is empty
   */
  isEmpty(): Result<boolean, QiError>

  /**
   * Check if queue is full
   */
  isFull(): Result<boolean, QiError>

  /**
   * Clear all messages from queue
   */
  clear(): Result<number, QiError> // Returns number of cleared messages

  /**
   * Pause message processing
   */
  pause(): Result<void, QiError>

  /**
   * Resume message processing
   */
  resume(): Result<void, QiError>

  /**
   * Check if queue is paused
   */
  isPaused(): Result<boolean, QiError>

  /**
   * Cleanup and destroy queue
   */
  destroy(): Promise<Result<void, QiError>>
}

/**
 * Priority-aware message queue interface
 */
export interface IPriorityMessageQueue<T extends QiMessage = QiMessage>
  extends IAsyncMessageQueue<T> {
  /**
   * Enqueue message with specific priority override
   */
  enqueueWithPriority(message: T, priority: MessagePriority): Result<void, QiError>

  /**
   * Get messages by priority level
   */
  getMessagesByPriority(priority: MessagePriority): Result<readonly T[], QiError>

  /**
   * Get count of messages by priority
   */
  getCountByPriority(): Result<Record<MessagePriority, number>, QiError>

  /**
   * Promote message priority
   */
  promoteMessage(messageId: string, newPriority: MessagePriority): Result<boolean, QiError>
}

/**
 * Filtered message queue interface for type-specific queues
 */
export interface IFilteredMessageQueue<T extends QiMessage = QiMessage>
  extends IAsyncMessageQueue<T> {
  /**
   * Set message filter predicate
   */
  setFilter(filter: (message: QiMessage) => message is T): Result<void, QiError>

  /**
   * Clear message filter
   */
  clearFilter(): Result<void, QiError>

  /**
   * Check if message passes current filter
   */
  passesFilter(message: QiMessage): Result<boolean, QiError>
}

/**
 * Batch message queue interface for bulk operations
 */
export interface IBatchMessageQueue<T extends QiMessage = QiMessage> extends IAsyncMessageQueue<T> {
  /**
   * Enqueue multiple messages at once
   */
  enqueueBatch(messages: readonly T[]): Result<number, QiError> // Returns number successfully queued

  /**
   * Dequeue multiple messages at once
   */
  dequeueBatch(count: number): Result<readonly T[], QiError>

  /**
   * Process messages in batches
   */
  processBatch(
    processor: (batch: readonly T[]) => Promise<Result<void, QiError>>,
    batchSize: number
  ): AsyncGenerator<Result<void, QiError>, void, unknown>
}

/**
 * Persistent message queue interface for durability
 */
export interface IPersistentMessageQueue<T extends QiMessage = QiMessage>
  extends IAsyncMessageQueue<T> {
  /**
   * Persist queue state to storage
   */
  persist(): Promise<Result<void, QiError>>

  /**
   * Restore queue state from storage
   */
  restore(): Promise<Result<void, QiError>>

  /**
   * Set persistence options
   */
  setPersistenceOptions(options: {
    autoSave?: boolean
    saveInterval?: number
    storageKey?: string
  }): Result<void, QiError>

  /**
   * Get persistence status
   */
  getPersistenceStatus(): Result<
    {
      enabled: boolean
      lastSaved: Date | null
      pendingChanges: number
    },
    QiError
  >
}

/**
 * Observable message queue interface for event-driven patterns
 */
export interface IObservableMessageQueue<T extends QiMessage = QiMessage>
  extends IAsyncMessageQueue<T> {
  /**
   * Subscribe to queue events
   */
  subscribe(eventType: QueueEventType, callback: QueueEventCallback<T>): Result<() => void, QiError>

  /**
   * Emit queue event
   */
  emit(eventType: QueueEventType, data: unknown): Result<void, QiError>

  /**
   * Get active subscription count
   */
  getSubscriptionCount(): Result<number, QiError>
}

/**
 * Queue event types for observable pattern
 */
export enum QueueEventType {
  MESSAGE_ENQUEUED = 'message_enqueued',
  MESSAGE_DEQUEUED = 'message_dequeued',
  MESSAGE_PROCESSED = 'message_processed',
  MESSAGE_FAILED = 'message_failed',
  QUEUE_FULL = 'queue_full',
  QUEUE_EMPTY = 'queue_empty',
  QUEUE_PAUSED = 'queue_paused',
  QUEUE_RESUMED = 'queue_resumed',
  QUEUE_ERROR = 'queue_error',
  QUEUE_DESTROYED = 'queue_destroyed',
}

/**
 * Queue event callback type
 */
export type QueueEventCallback<T extends QiMessage = QiMessage> = (event: {
  type: QueueEventType
  timestamp: Date
  queue: IAsyncMessageQueue<T>
  data?: unknown
  message?: T
  error?: QiError
}) => void | Promise<void>
