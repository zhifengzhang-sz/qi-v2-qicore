# Async Message Queue API Reference

The Async Message Queue module provides a h2A-inspired message queue system with QiCore functional programming patterns. It enables non-blocking message processing with Promise-based flow control and priority-based message handling.

## Core Interface

### IAsyncMessageQueue<T>

Primary interface for async message queue operations.

```typescript
interface IAsyncMessageQueue<T = QiMessage> extends AsyncIterable<T> {
  // Core queue operations
  enqueue(message: T, priority?: MessagePriority): Result<void, QiError>
  read(): Promise<Result<IteratorResult<T>, QiError>>
  
  // Queue lifecycle
  start(): Result<void, QiError>
  stop(): Result<void, QiError>
  dispose(): Promise<Result<void, QiError>>
  
  // Queue inspection
  getState(): QueueState
  getStats(): MessageStats
  getQueueLength(): number
  
  // Queue management
  clear(): Result<void, QiError>
  isPaused(): boolean
  resume(): Result<void, QiError>
  pause(): Result<void, QiError>
}
```

### QueueState

Represents the current state of the message queue.

```typescript
interface QueueState {
  readonly started: boolean
  readonly isDone: boolean
  readonly hasError: boolean
  readonly messageCount: number
  readonly processingCount: number
  readonly errorCount: number
}
```

### QueueOptions

Configuration options for creating message queues.

```typescript
interface QueueOptions {
  maxSize?: number                    // Maximum queue size (0 = unlimited)
  priorityMode?: boolean              // Enable priority-based processing
  autoStart?: boolean                 // Auto-start the queue
  autoCleanup?: boolean              // Auto-cleanup finished messages
  cleanupInterval?: number           // Cleanup interval in ms
  enableStats?: boolean              // Enable statistics collection
  onMessage?: QueueEventCallback     // Message event handler
  onError?: QueueEventCallback       // Error event handler  
  onStateChange?: QueueEventCallback // State change handler
  cleanupFn?: () => void            // Custom cleanup function
}
```

## Message Type System

### MessageType

Core message types supported by the system.

```typescript
enum MessageType {
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
  STREAM_ERROR = 'stream_error'
}
```

### MessagePriority

Priority levels for message processing.

```typescript
enum MessagePriority {
  CRITICAL = 0, // System control, abort signals
  HIGH = 1,     // User interrupts, errors
  NORMAL = 2,   // Regular commands and responses
  LOW = 3       // Background processing, logging
}
```

### MessageStatus

Processing status for messages.

```typescript
enum MessageStatus {
  PENDING = 'pending',       // Queued for processing
  PROCESSING = 'processing', // Currently being processed
  COMPLETED = 'completed',   // Successfully processed
  FAILED = 'failed',         // Processing failed
  CANCELLED = 'cancelled'    // Processing was cancelled
}
```

### BaseMessage

Base interface for all messages.

```typescript
interface BaseMessage {
  readonly id: string
  readonly timestamp: Date
  readonly type: MessageType
  readonly priority: MessagePriority
  readonly status: MessageStatus
  readonly data?: unknown
  readonly context?: Record<string, unknown>
  readonly error?: QiError
}
```

### QiMessage

Union type for all supported message types.

```typescript
type QiMessage = 
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
```

## Factory Functions

### QiAsyncMessageQueue<T>(options?: QueueOptions): QiAsyncMessageQueue<T>

Creates a new message queue instance.

```typescript
const queue = new QiAsyncMessageQueue<QiMessage>({
  maxSize: 1000,
  priorityMode: true,
  autoStart: true,
  enableStats: true
})
```

### QiMessageFactory

Utility factory for creating typed messages.

```typescript
namespace QiMessageFactory {
  // Command messages
  createCommand(command: string, args?: unknown[]): CommandMessage
  createCommandResponse(commandId: string, result: unknown): CommandResponseMessage
  createCommandError(commandId: string, error: QiError): CommandErrorMessage
  
  // User interaction messages  
  createUserInput(input: string): UserInputMessage
  createUserInterrupt(reason?: string): UserInterruptMessage
  
  // Agent messages
  createAgentOutput(output: string): AgentOutputMessage
  createAgentThinking(thought: string): AgentThinkingMessage
  createAgentError(error: QiError): AgentErrorMessage
  
  // System messages
  createSystemControl(command: string): SystemControlMessage
  createSystemStatus(status: unknown): SystemStatusMessage
  createSystemAbort(reason: string): SystemAbortMessage
  
  // Stream messages
  createStreamStart(streamId: string): StreamStartMessage
  createStreamData(streamId: string, data: unknown): StreamDataMessage
  createStreamEnd(streamId: string): StreamEndMessage
  createStreamError(streamId: string, error: QiError): StreamErrorMessage
}
```

## Usage Examples

### Basic Queue Usage

```typescript
import { QiAsyncMessageQueue, QiMessageFactory } from '@qi/amsg'

// Create and start queue
const queue = new QiAsyncMessageQueue({
  maxSize: 100,
  priorityMode: true,
  autoStart: true
})

// Enqueue messages
const commandMsg = QiMessageFactory.createCommand('run-tests')
queue.enqueue(commandMsg, MessagePriority.HIGH)

// Process messages
for await (const message of queue) {
  match(
    (msg) => {
      console.log('Processing message:', msg.type)
      // Process message...
    },
    (error) => console.error('Queue error:', error.message),
    await queue.read()
  )
}
```

### Priority-Based Processing

```typescript
const queue = new QiAsyncMessageQueue({ priorityMode: true })

// Critical system messages processed first
queue.enqueue(
  QiMessageFactory.createSystemAbort('Emergency stop'),
  MessagePriority.CRITICAL
)

// High priority user interrupts
queue.enqueue(
  QiMessageFactory.createUserInterrupt('Ctrl+C'),
  MessagePriority.HIGH
)

// Normal priority commands
queue.enqueue(
  QiMessageFactory.createCommand('list-files'),
  MessagePriority.NORMAL
)
```

### Error Handling

```typescript
const queue = new QiAsyncMessageQueue({
  onError: (error) => {
    console.error('Queue error:', error.message)
  }
})

// All operations return Result<T>
const enqueueResult = queue.enqueue(message)
match(
  () => console.log('Message enqueued successfully'),
  (error) => console.error('Failed to enqueue:', error.message),
  enqueueResult
)
```

## Performance Characteristics

- **Enqueue**: O(log n) with priority queue, O(1) without priority
- **Dequeue**: O(log n) with priority queue, O(1) without priority
- **Memory**: O(n) where n is the number of messages
- **Thread Safety**: Single-threaded (JavaScript), uses async/await patterns

## Integration with QiCore

The async message queue integrates seamlessly with QiCore patterns:

- All operations return `Result<T, QiError>`
- Uses functional error handling throughout
- Supports async composition with `flatMapAsync`
- Compatible with QiCore logging and configuration modules

## Error Types

Messages queue operations can fail with specific error categories:

```typescript
// Queue capacity errors
const capacityError = queueError('Queue is full', { 
  operation: 'enqueue',
  queueSize: 1000 
})

// Message validation errors  
const validationError = queueError('Invalid message format', {
  operation: 'enqueue',
  messageType: 'unknown'
})

// Queue lifecycle errors
const lifecycleError = queueError('Queue not started', {
  operation: 'enqueue',
  state: 'stopped'
})
```

All errors follow QiCore error handling patterns and can be composed with other Result<T> operations.