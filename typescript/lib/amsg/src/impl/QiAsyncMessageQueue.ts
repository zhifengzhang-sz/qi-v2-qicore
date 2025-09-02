/**
 * QiAsyncMessageQueue Implementation
 *
 * h2A-inspired async message queue with QiCore Result<T> patterns.
 * Provides non-blocking message processing with Promise-based flow control.
 */

import type { IAsyncMessageQueue, QueueOptions, QueueState } from '../interfaces/IAsyncMessageQueue'
// v-0.6.1: QueueEventCallback and QueueEventType removed - pure message-driven
import type { MessageStats, QiMessage } from '../types/MessageTypes'
import { MessagePriority } from '../types/MessageTypes'
import { MessageStatus } from '../types/MessageTypes'
import {
  create,
  failure,
  flatMap,
  fromAsyncTryCatch,
  match,
  type QiError,
  type Result,
  success,
} from '@qi/base'
/**
 * Queue event types for subscription system
 */
type QueueEventType =
  | 'message_enqueued'
  | 'message_dequeued'
  | 'message_processed'
  | 'message_failed'
  | 'message_expired'
  | 'message_dropped'
  | 'queue_paused'
  | 'queue_resumed'
  | 'queue_cleared'
  | 'queue_error'

/**
 * Queue event data structure
 */
interface QueueEvent<T extends QiMessage = QiMessage> {
  type: QueueEventType
  message?: T
  error?: QiError
  reason?: string
  clearedCount?: number
  timestamp?: Date
}

/**
 * Queue subscriber callback type
 */
type QueueSubscriber<T extends QiMessage = QiMessage> = (event: QueueEvent<T>) => void

// Simple debug logger for messaging
const createDebugLogger = (name: string) => ({
  debug: (...args: any[]) => console.debug(`[${name}]`, ...args),
  info: (...args: any[]) => console.info(`[${name}]`, ...args),
  warn: (...args: any[]) => console.warn(`[${name}]`, ...args),
  error: (...args: any[]) => console.error(`[${name}]`, ...args),
  log: (...args: any[]) => console.log(`[${name}]`, ...args),
})

/**
 * Queue error types
 */
interface QueueError extends QiError {
  context: {
    operation?: string
    queueSize?: number
    messageId?: string
    messageType?: string
    timestamp?: string
    queueLength?: number
  }
}

// Message queue specific error factories with granular categorization
const queueError = {
  // Message validation errors
  invalidMessage: (messageId: string, reason: string): QueueError =>
    create('INVALID_MESSAGE', `Invalid message: ${messageId}`, 'VALIDATION', {
      messageId,
      reason,
      timestamp: new Date().toISOString(),
    }) as QueueError,

  // Queue state errors
  alreadyStarted: (operation: string): QueueError =>
    create('QUEUE_ALREADY_STARTED', `Queue already started - cannot ${operation}`, 'VALIDATION', {
      operation,
      timestamp: new Date().toISOString(),
    }) as QueueError,

  queueDone: (messageId: string): QueueError =>
    create('QUEUE_DONE', 'Cannot enqueue to completed queue', 'VALIDATION', {
      messageId,
      timestamp: new Date().toISOString(),
    }) as QueueError,

  queueInError: (messageId: string): QueueError =>
    create('QUEUE_ERROR', 'Cannot enqueue to error queue', 'VALIDATION', {
      messageId,
      timestamp: new Date().toISOString(),
    }) as QueueError,

  // Capacity errors
  queueFull: (messageId: string, maxSize: number, currentSize: number): QueueError =>
    create('QUEUE_FULL', 'Queue has reached maximum size', 'RESOURCE', {
      messageId,
      maxSize,
      currentSize,
      timestamp: new Date().toISOString(),
    }) as QueueError,

  // System errors
  queueDestroyed: (): QueueError =>
    create('QUEUE_DESTROYED', 'Queue was destroyed', 'SYSTEM', {
      timestamp: new Date().toISOString(),
    }) as QueueError,

  cleanupFailed: (reason: string): QueueError =>
    create('CLEANUP_FAILED', `Cleanup function failed: ${reason}`, 'SYSTEM', {
      reason,
      timestamp: new Date().toISOString(),
    }) as QueueError,

  // Processing backpressure error
  processingBackpressure: (
    messageId: string,
    currentProcessing: number,
    maxConcurrent: number
  ): QueueError =>
    create('PROCESSING_BACKPRESSURE', 'Queue processing at maximum capacity', 'RESOURCE', {
      messageId,
      currentProcessing,
      maxConcurrent,
      timestamp: new Date().toISOString(),
    }) as QueueError,

  // Legacy system error for backward compatibility
  systemError: (code: string, message: string, context: QueueError['context'] = {}): QueueError =>
    create(code, message, 'SYSTEM', {
      timestamp: new Date().toISOString(),
      queueLength: 0,
      ...context,
    }) as QueueError,
}

/**
 * Internal message wrapper for queue management
 */
interface QueuedMessage<T extends QiMessage = QiMessage> {
  readonly message: T
  readonly enqueuedAt: Date
  readonly expiresAt?: Date
  status: MessageStatus
  processingStarted?: Date
  processingCompleted?: Date
  processingError?: QiError
}

/**
 * QiAsyncMessageQueue implementation following h2A pattern with priority queuing
 */
export class QiAsyncMessageQueue<T extends QiMessage = QiMessage> implements IAsyncMessageQueue<T> {
  // Priority-based queues for efficient message ordering
  private priorityQueues: Map<MessagePriority, QueuedMessage<T>[]> = new Map()
  private readResolve?: (value: IteratorResult<T, any>) => void
  private readReject?: (error: any) => void
  private state: QueueState
  private options: Required<QueueOptions>
  private stats: MessageStats
  // v-0.6.1: EventEmitter patterns removed - pure message-driven
  private cleanupTimer?: NodeJS.Timeout
  private isPausedState = false
  private debug = createDebugLogger('QiAsyncMessageQueue')
  // New: Subscription system for monitoring
  private subscribers: Set<QueueSubscriber<T>> = new Set()
  // New: Backpressure management
  private processingCount = 0
  private droppedMessageCount = 0

  constructor(options: QueueOptions = {}) {
    // Initialize priority queues for each priority level
    this.priorityQueues.set(MessagePriority.CRITICAL, [])
    this.priorityQueues.set(MessagePriority.HIGH, [])
    this.priorityQueues.set(MessagePriority.NORMAL, [])
    this.priorityQueues.set(MessagePriority.LOW, [])

    // Initialize state
    this.state = {
      started: false,
      isDone: false,
      hasError: false,
      messageCount: 0,
      processingCount: 0,
      errorCount: 0,
    }

    // Set default options
    this.options = {
      maxSize: options.maxSize || 0, // 0 = unlimited
      maxConcurrent: options.maxConcurrent || 10,
      messageTtl: options.messageTtl || 300000, // 5 minutes default
      priorityQueuing: options.priorityQueuing ?? true,
      autoCleanup: options.autoCleanup ?? true,
      enableStats: options.enableStats ?? true,
      cleanupFn: options.cleanupFn || (() => {}),
    }

    // Initialize stats
    this.stats = {
      totalMessages: 0,
      messagesByType: {} as any,
      messagesByPriority: {} as any,
      messagesByStatus: {} as any,
      averageProcessingTime: 0,
      errorRate: 0,
      queueLength: 0,
    }

    // Setup auto-cleanup if enabled
    if (this.options.autoCleanup) {
      this.setupAutoCleanup()
    }
  }

  /**
   * AsyncIterable implementation - core h2A pattern
   */
  [Symbol.asyncIterator](): AsyncIterator<T, any, undefined> {
    this.debug.log(`Creating async iterator, started: ${this.state.started}`)
    if (this.state.started) {
      this.debug.error(`CRITICAL ERROR: Queue already started - this causes infinite loops!`)
      this.debug.error(`Stack trace:`, new Error().stack)
      const error = queueError.alreadyStarted('iterate')
      throw error
    }

    this.state = { ...this.state, started: true }
    // v-0.6.1: Event emission removed - pure message-driven
    this.debug.log(`Async iterator created successfully`)

    return this
  }

  /**
   * Core async iterator method - implements h2A's next() pattern
   * QiCore Interface Boundary Pattern: Use QiCore internally, adapt to standard interface
   */
  async next(): Promise<IteratorResult<T, any>> {
    const result = await this.nextQiCore()

    // Interface boundary: unwrap QiCore Result<T> to standard interface
    return match(
      (iteratorResult) => iteratorResult,
      (error) => {
        // At interface boundaries, convert QiError to standard exception
        // This is the ONLY acceptable place to throw in QiCore architecture
        throw new Error(`Iterator error: ${error.message}`)
      },
      result
    )
  }

  /**
   * Internal QiCore implementation - pure functional with Result<T>
   */
  private async nextQiCore(): Promise<Result<IteratorResult<T, any>, QiError>> {
    return fromAsyncTryCatch(
      async () => {
        // Check if queue is paused
        if (this.isPausedState) {
          return new Promise<IteratorResult<T, any>>((resolve) => {
            const checkPaused = () => {
              if (!this.isPausedState) {
                this.next().then(resolve)
              } else {
                setTimeout(checkPaused, 100)
              }
            }
            checkPaused()
          })
        }

        // Priority: return from queue if available
        const nextMessage = this.dequeueNextMessage()
        return match(
          (queuedMessage) => {
            if (queuedMessage) {
              this.debug.log(
                `Dequeuing message ID: ${queuedMessage.message.id}, type: ${queuedMessage.message.type}`
              )
              this.updateMessageStatus(queuedMessage, MessageStatus.PROCESSING)
              // v-0.6.1: Event emission removed - pure message-driven

              return Promise.resolve({
                done: false,
                value: queuedMessage.message,
              })
            }
            // Continue to check done/error states
            return this.handleEmptyQueue()
          },
          (error) => {
            // QiCore error handling - propagate QiError properly
            throw error
          },
          nextMessage
        )
      },
      (error: unknown) =>
        queueError.systemError(
          'ITERATOR_ERROR',
          `Iterator next() failed: ${error instanceof Error ? error.message : String(error)}`,
          { operation: 'next' }
        )
    )
  }

  /**
   * Internal QiCore method for handling empty queue state
   */
  private async handleEmptyQueue(): Promise<IteratorResult<T, any>> {
    const result = await fromAsyncTryCatch(
      async () => {
        // Check if queue is done
        if (this.state.isDone) {
          // v-0.6.1: Event emission removed - pure message-driven
          return {
            done: true,
            value: undefined,
          } as IteratorResult<T, any>
        }

        // Check for error state
        if (this.state.hasError) {
          throw queueError.systemError('QUEUE_ERROR', 'Queue is in error state')
        }

        // Wait for new message - core h2A non-blocking pattern
        return new Promise<IteratorResult<T, any>>((resolve, reject) => {
          this.readResolve = resolve
          this.readReject = reject
        })
      },
      (error: unknown) =>
        queueError.systemError(
          'EMPTY_QUEUE_ERROR',
          `Empty queue handling failed: ${error instanceof Error ? error.message : String(error)}`,
          { operation: 'handleEmptyQueue' }
        )
    )

    // Since this is called internally, propagate QiError as exception for the outer boundary to handle
    return match(
      (iteratorResult) => iteratorResult,
      (error) => {
        // Internal method - propagate QiError as exception to be caught by boundary
        throw error
      },
      result
    )
  }

  /**
   * Enqueue message - supports real-time injection (h2A pattern) with backpressure management
   */
  enqueue(message: T): Result<void, QueueError> {
    this.debug.log(
      `Enqueuing message ID: ${message.id}, type: ${message.type}, priority: ${message.priority}`
    )

    // Validate queue state
    if (this.state.isDone) {
      return failure(queueError.queueDone(message.id))
    }

    if (this.state.hasError) {
      return failure(queueError.queueInError(message.id))
    }

    // Backpressure management: Check size limits with priority handling
    const totalQueueSize = this.getTotalQueueSize()
    if (this.options.maxSize > 0 && totalQueueSize >= this.options.maxSize) {
      // Critical messages always accepted - drop lowest priority if needed
      if (message.priority === MessagePriority.CRITICAL) {
        this.makeRoomForCriticalMessage()
      } else {
        this.droppedMessageCount++
        this.notifySubscribers({ type: 'message_dropped', message, reason: 'queue_full' })
        return failure(queueError.queueFull(message.id, this.options.maxSize, totalQueueSize))
      }
    }

    // Check processing backpressure
    if (
      this.processingCount >= this.options.maxConcurrent &&
      message.priority > MessagePriority.HIGH
    ) {
      this.droppedMessageCount++
      this.notifySubscribers({
        type: 'message_dropped',
        message,
        reason: 'processing_backpressure',
      })
      return failure(
        queueError.processingBackpressure(
          message.id,
          this.processingCount,
          this.options.maxConcurrent
        )
      )
    }

    // Create queued message (not inserted yet to avoid duplication when a reader is waiting)
    const queuedMessage: QueuedMessage<T> = {
      message,
      enqueuedAt: new Date(),
      expiresAt:
        this.options.messageTtl > 0 ? new Date(Date.now() + this.options.messageTtl) : undefined,
      status: MessageStatus.PENDING,
    }

    // Update state and stats (consistent regardless of delivery path)
    this.state = {
      ...this.state,
      messageCount: this.state.messageCount + 1,
    }
    this.updateStats(message, 'enqueued')
    // v-0.6.1: Event emission removed - pure message-driven

    // If there's a waiting reader, resolve immediately WITHOUT inserting into queue
    if (this.readResolve && !this.isPausedState) {
      const resolve = this.readResolve
      this.readResolve = undefined
      this.readReject = undefined

      this.updateMessageStatus(queuedMessage, MessageStatus.PROCESSING)
      // v-0.6.1: Event emission removed - pure message-driven

      resolve({
        done: false,
        value: message,
      })

      return success(undefined)
    }

    // No waiting reader - insert into appropriate priority queue
    const priorityQueue = this.priorityQueues.get(message.priority)
    if (!priorityQueue) {
      return failure(queueError.invalidMessage(message.id, `Invalid priority: ${message.priority}`))
    }

    priorityQueue.push(queuedMessage)
    this.notifySubscribers({ type: 'message_enqueued', message })

    return success(undefined)
  }

  /**
   * Mark queue as done - no more messages will be added
   */
  done(): Result<void, QueueError> {
    if (this.state.isDone) {
      return success(undefined)
    }

    this.state = { ...this.state, isDone: true }

    // Resolve waiting reader if queue is empty
    if (this.readResolve && this.getTotalQueueSize() === 0) {
      const resolve = this.readResolve
      this.readResolve = undefined
      this.readReject = undefined

      resolve({
        done: true,
        value: undefined,
      })
    }

    // v-0.6.1: Event emission removed - pure message-driven
    return success(undefined)
  }

  /**
   * Signal error condition
   */
  error(error: QiError): Result<void, QueueError> {
    this.state = {
      ...this.state,
      hasError: true,
      errorCount: this.state.errorCount + 1,
    }

    // Reject waiting reader
    if (this.readReject) {
      const reject = this.readReject
      this.readResolve = undefined
      this.readReject = undefined
      reject(error)
    }

    // v-0.6.1: Event emission removed - pure message-driven
    return success(undefined)
  }

  /**
   * Get current queue state
   */
  getState(): Result<QueueState, QueueError> {
    return success({
      ...this.state,
      messageCount: this.getTotalQueueSize(),
    })
  }

  /**
   * Get queue statistics
   */
  getStats(): Result<MessageStats | null, QueueError> {
    if (!this.options.enableStats) {
      return success(null)
    }

    return success({
      ...this.stats,
      queueLength: this.getTotalQueueSize(),
    })
  }

  /**
   * Peek at next message without removing it
   */
  peek(): Result<T | null, QueueError> {
    if (this.getTotalQueueSize() === 0) {
      return success(null)
    }

    const nextMessage = this.findNextMessage()
    return success(nextMessage ? nextMessage.message : null)
  }

  /**
   * Get queue size
   */
  size(): Result<number, QueueError> {
    return success(this.getTotalQueueSize())
  }

  /**
   * Check if queue is empty
   */
  isEmpty(): Result<boolean, QueueError> {
    return success(this.getTotalQueueSize() === 0)
  }

  /**
   * Check if queue is full
   */
  isFull(): Result<boolean, QueueError> {
    if (this.options.maxSize === 0) {
      return success(false)
    }
    return success(this.getTotalQueueSize() >= this.options.maxSize)
  }

  /**
   * Clear all messages from queue
   */
  clear(): Result<number, QueueError> {
    const clearedCount = this.getTotalQueueSize()

    // Clear all priority queues
    for (const queue of this.priorityQueues.values()) {
      queue.length = 0
    }

    this.state = {
      ...this.state,
      messageCount: 0,
    }

    this.notifySubscribers({ type: 'queue_cleared', clearedCount })
    return success(clearedCount)
  }

  /**
   * Pause message processing
   */
  pause(): Result<void, QueueError> {
    this.isPausedState = true
    // v-0.6.1: Event emission removed - pure message-driven
    return success(undefined)
  }

  /**
   * Resume message processing
   */
  resume(): Result<void, QueueError> {
    this.isPausedState = false
    // v-0.6.1: Event emission removed - pure message-driven
    return success(undefined)
  }

  /**
   * Check if queue is paused
   */
  isPaused(): Result<boolean, QueueError> {
    return success(this.isPausedState)
  }

  /**
   * Cleanup and destroy queue
   */
  async destroy(): Promise<Result<void, QueueError>> {
    // Clear cleanup timer
    if (this.cleanupTimer) {
      clearInterval(this.cleanupTimer)
      this.cleanupTimer = undefined
    }

    // Reject any waiting readers
    if (this.readReject) {
      this.readReject(queueError.queueDestroyed())
      this.readResolve = undefined
      this.readReject = undefined
    }

    // Clear all priority queues
    for (const queue of this.priorityQueues.values()) {
      queue.length = 0
    }

    // Call cleanup function if provided - use QiCore pattern
    if (this.options.cleanupFn) {
      const cleanupResult = await fromAsyncTryCatch(
        async () => {
          await this.options.cleanupFn?.()
          return undefined // Explicit return for TypeScript
        },
        (error: unknown) =>
          queueError.cleanupFailed(
            `destroy operation: ${error instanceof Error ? error.message : String(error)}`
          )
      )

      // Handle cleanup result using proper functional composition
      return flatMap(
        () => success(undefined), // Success - continue with destroy
        cleanupResult
      )
    }

    // v-0.6.1: Event subscriptions removed - pure message-driven

    // v-0.6.1: Event emission removed - pure message-driven
    return success(undefined)
  }

  // Private helper methods

  private dequeueNextMessage(): Result<QueuedMessage<T> | null, QueueError> {
    // Dequeue from highest priority queue first (CRITICAL=0 to LOW=3)
    for (const priority of [
      MessagePriority.CRITICAL,
      MessagePriority.HIGH,
      MessagePriority.NORMAL,
      MessagePriority.LOW,
    ]) {
      const queue = this.priorityQueues.get(priority)
      if (!queue || queue.length === 0) continue

      const message = queue.shift()!

      // Check if message has expired
      if (message.expiresAt && new Date() > message.expiresAt) {
        this.updateStats(message.message, 'expired')
        this.notifySubscribers({ type: 'message_expired', message: message.message })
        // Use functional composition for recursive call
        return this.dequeueNextMessage()
      }

      this.notifySubscribers({ type: 'message_dequeued', message: message.message })
      return success(message)
    }

    return success(null)
  }

  private findNextMessage(): QueuedMessage<T> | null {
    // Check highest priority queues first
    for (const priority of [
      MessagePriority.CRITICAL,
      MessagePriority.HIGH,
      MessagePriority.NORMAL,
      MessagePriority.LOW,
    ]) {
      const queue = this.priorityQueues.get(priority)
      if (!queue || queue.length === 0) continue

      // Find first non-expired message in this priority queue
      for (const message of queue) {
        if (!message.expiresAt || new Date() <= message.expiresAt) {
          return message
        }
      }
    }

    return null
  }

  /**
   * Get total size across all priority queues
   */
  private getTotalQueueSize(): number {
    return Array.from(this.priorityQueues.values()).reduce(
      (total, queue) => total + queue.length,
      0
    )
  }

  /**
   * Make room for critical messages by dropping lowest priority messages
   */
  private makeRoomForCriticalMessage(): void {
    // Drop from lowest priority first
    for (const priority of [MessagePriority.LOW, MessagePriority.NORMAL, MessagePriority.HIGH]) {
      const queue = this.priorityQueues.get(priority)
      if (queue && queue.length > 0) {
        const dropped = queue.pop()! // Remove last (oldest) message
        this.droppedMessageCount++
        this.notifySubscribers({
          type: 'message_dropped',
          message: dropped.message,
          reason: 'critical_priority_override',
        })
        break
      }
    }
  }

  private updateMessageStatus(queuedMessage: QueuedMessage<T>, status: MessageStatus): void {
    queuedMessage.status = status

    if (status === MessageStatus.PROCESSING) {
      queuedMessage.processingStarted = new Date()
      this.processingCount++
      this.state = {
        ...this.state,
        processingCount: this.state.processingCount + 1,
      }
    } else if (status === MessageStatus.COMPLETED || status === MessageStatus.FAILED) {
      queuedMessage.processingCompleted = new Date()
      this.processingCount = Math.max(0, this.processingCount - 1)
      this.state = {
        ...this.state,
        processingCount: Math.max(0, this.state.processingCount - 1),
      }
      this.notifySubscribers({
        type: status === MessageStatus.COMPLETED ? 'message_processed' : 'message_failed',
        message: queuedMessage.message,
      })
    }

    this.updateStats(queuedMessage.message, status)
  }

  private updateStats(message: T, event: string | MessageStatus): void {
    if (!this.options.enableStats) {
      return
    }

    // Update counters
    if (event === 'enqueued') {
      this.stats.totalMessages++
      this.stats.messagesByType[message.type] = (this.stats.messagesByType[message.type] || 0) + 1
      this.stats.messagesByPriority[message.priority] =
        (this.stats.messagesByPriority[message.priority] || 0) + 1
    }

    if (typeof event === 'string' && event in MessageStatus) {
      const status = event as MessageStatus
      this.stats.messagesByStatus[status] = (this.stats.messagesByStatus[status] || 0) + 1
    }

    // Calculate error rate
    const totalErrors = this.stats.messagesByStatus[MessageStatus.FAILED] || 0
    this.stats.errorRate = this.stats.totalMessages > 0 ? totalErrors / this.stats.totalMessages : 0
  }

  private setupAutoCleanup(): void {
    this.cleanupTimer = setInterval(() => {
      this.cleanupExpiredMessages()
    }, 60000) // Cleanup every minute
  }

  private cleanupExpiredMessages(): void {
    const now = new Date()
    let totalRemovedCount = 0

    // Clean up expired messages from all priority queues
    for (const [priority, queue] of this.priorityQueues.entries()) {
      const originalLength = queue.length

      this.priorityQueues.set(
        priority,
        queue.filter((queuedMessage) => {
          if (queuedMessage.expiresAt && now > queuedMessage.expiresAt) {
            this.updateStats(queuedMessage.message, 'expired')
            this.notifySubscribers({ type: 'message_expired', message: queuedMessage.message })
            return false
          }
          return true
        })
      )

      totalRemovedCount += originalLength - this.priorityQueues.get(priority)!.length
    }

    if (totalRemovedCount > 0) {
      this.state = {
        ...this.state,
        messageCount: this.getTotalQueueSize(),
      }
    }
  }

  /**
   * Subscribe to queue events for monitoring
   */
  subscribe(callback: QueueSubscriber<T>): () => void {
    this.subscribers.add(callback)

    // Return unsubscribe function
    return () => {
      this.subscribers.delete(callback)
    }
  }

  /**
   * Get queue statistics with priority breakdown
   */
  getDetailedStats(): Result<
    | (MessageStats & {
        droppedMessages: number
        queuesByPriority: Record<MessagePriority, number>
        processingCount: number
      })
    | null,
    QueueError
  > {
    if (!this.options.enableStats) {
      return success(null)
    }

    const queuesByPriority = {
      [MessagePriority.CRITICAL]: this.priorityQueues.get(MessagePriority.CRITICAL)?.length || 0,
      [MessagePriority.HIGH]: this.priorityQueues.get(MessagePriority.HIGH)?.length || 0,
      [MessagePriority.NORMAL]: this.priorityQueues.get(MessagePriority.NORMAL)?.length || 0,
      [MessagePriority.LOW]: this.priorityQueues.get(MessagePriority.LOW)?.length || 0,
    }

    return success({
      ...this.stats,
      queueLength: this.getTotalQueueSize(),
      droppedMessages: this.droppedMessageCount,
      queuesByPriority,
      processingCount: this.processingCount,
    })
  }

  /**
   * Notify all subscribers of queue events
   */
  private notifySubscribers(event: QueueEvent<T>): void {
    const eventWithTimestamp = {
      ...event,
      timestamp: event.timestamp || new Date(),
    }

    for (const subscriber of this.subscribers) {
      try {
        subscriber(eventWithTimestamp)
      } catch (error) {
        this.debug.error('Subscriber error:', error)
      }
    }
  }

  // v-0.6.1: emit method completely removed - pure message-driven architecture
}
