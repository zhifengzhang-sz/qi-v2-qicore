# lib/amsg Architecture

The `lib/amsg` module provides **asynchronous message queue system** built on QiCore functional programming patterns, implementing h2A-inspired message processing for CLI and agent communication workflows.

## Overview

This module implements a priority-based async message queue system with typed message handling, designed for command-line interfaces and agent interactions. All operations use `Result<T>` patterns and functional composition from `lib/base`.

## Module Structure

```
lib/amsg/src/
├── index.ts                    # Main exports and convenience re-exports
├── interfaces/
│   ├── index.ts               # Interface re-exports
│   ├── IAsyncMessageQueue.ts  # Core queue interface
│   └── IMessage.ts           # Message handling interfaces
├── types/
│   ├── index.ts              # Type re-exports
│   ├── MessageTypes.ts       # Core message type system
│   └── CLIMessageTypes.ts    # CLI-specific message types
└── impl/
    ├── index.ts              # Implementation re-exports
    ├── QiAsyncMessageQueue.ts # Main queue implementation
    └── QiMessageFactory.ts   # Message factory functions
```

## Architecture Principles

### 1. Message-Driven Design
- **Async Iterator Pattern**: Queue implements `AsyncIterable<T>`
- **Priority-Based Processing**: Critical messages processed first
- **Type-Safe Messages**: Strongly typed message union types
- **Event-Driven**: Optional event callbacks for monitoring

### 2. Functional Programming Integration
- **Result<T> Returns**: All operations return `Result<T, QiError>`
- **Composable Operations**: Queue operations chain with `flatMap`, `map`
- **No Side Effects**: Pure message processing functions
- **Immutable Messages**: Messages are readonly data structures

### 3. h2A-Inspired Patterns
- **Non-Blocking I/O**: Promise-based async processing
- **Backpressure Handling**: Queue size limits and flow control
- **Graceful Degradation**: Error handling with recovery strategies

## Core Architecture

### 1. Message Type System

```
┌─────────────────────────────────────────────────────────────┐
│                    Message Type Hierarchy                   │
├─────────────────────────────────────────────────────────────┤
│ BaseMessage (id, timestamp, type, priority, status)         │
│ ├── CommandMessage (command execution)                      │
│ ├── UserInputMessage (user interactions)                    │
│ ├── AgentOutputMessage (agent responses)                    │
│ ├── SystemControlMessage (system operations)                │
│ └── StreamMessage (streaming data)                          │
└─────────────────────────────────────────────────────────────┘
```

**Message Categories:**
- **Command Messages**: `COMMAND`, `COMMAND_RESPONSE`, `COMMAND_ERROR`
- **User Messages**: `USER_INPUT`, `USER_INTERRUPT`  
- **Agent Messages**: `AGENT_OUTPUT`, `AGENT_THINKING`, `AGENT_ERROR`
- **System Messages**: `SYSTEM_CONTROL`, `SYSTEM_STATUS`, `SYSTEM_ABORT`
- **Stream Messages**: `STREAM_START`, `STREAM_DATA`, `STREAM_END`, `STREAM_ERROR`

### 2. Priority System

```typescript
enum MessagePriority {
  CRITICAL = 0, // System control, abort signals (immediate processing)
  HIGH = 1,     // User interrupts, errors (high priority)
  NORMAL = 2,   // Regular commands and responses (default)
  LOW = 3       // Background processing, logging (deferred)
}
```

**Priority Handling:**
- **Priority Queue**: Messages sorted by priority + insertion order
- **Preemption**: Critical messages can interrupt normal processing
- **Starvation Prevention**: Low priority messages eventually processed

### 3. Queue State Management

```
┌─────────────────────────────────────────────────────────────┐
│                    Queue Lifecycle                         │
├─────────────────────────────────────────────────────────────┤
│ Created → Started → Processing ⇄ Paused → Stopped → Disposed │
│           ↑              ↓                    ↓              │
│           └── Error ─────┴────────────────────┘              │
└─────────────────────────────────────────────────────────────┘
```

**State Properties:**
- **started**: Queue is accepting and processing messages
- **isDone**: All messages processed and queue finished
- **hasError**: Queue encountered unrecoverable error
- **messageCount**: Total messages processed
- **processingCount**: Messages currently being processed
- **errorCount**: Total processing errors encountered

### 4. Async Iterator Implementation

```typescript
interface IAsyncMessageQueue<T> extends AsyncIterable<T> {
  // Core operations
  enqueue(message: T, priority?: MessagePriority): Result<void, QiError>
  read(): Promise<Result<IteratorResult<T>, QiError>>
  
  // Lifecycle management
  start(): Result<void, QiError>
  stop(): Result<void, QiError>
  dispose(): Promise<Result<void, QiError>>
}
```

**Iterator Protocol:**
- **Async Iteration**: `for await (const message of queue)`
- **Manual Reading**: `queue.read()` for fine-grained control
- **Backpressure**: Iterator pauses when consumer can't keep up

## Implementation Details

### 1. QiAsyncMessageQueue<T>

**Core Data Structures:**
```typescript
class QiAsyncMessageQueue<T> {
  private priorityQueues: Map<MessagePriority, QueuedMessage<T>[]>
  private stats: MessageStats
  private state: QueueState
  private events: EventEmitter
}
```

**Key Features:**
- **Priority Queues**: Separate queue per priority level
- **Statistics Tracking**: Hit rates, processing times, error rates
- **Event Emission**: Optional callbacks for monitoring
- **Graceful Cleanup**: Resource disposal with timeout handling

### 2. Message Factory System

```typescript
namespace QiMessageFactory {
  // Type-safe message creation
  createCommand(command: string, args?: unknown[]): CommandMessage
  createUserInput(input: string): UserInputMessage
  createAgentOutput(output: string): AgentOutputMessage
  createSystemAbort(reason: string): SystemAbortMessage
  
  // Utility functions
  generateId(): string // Timestamp + random suffix
  getPriorityForType(type: MessageType): MessagePriority
  validateMessage(message: unknown): Result<QiMessage, QiError>
}
```

### 3. CLI Message Extensions

**CLI-Specific Message Types:**
- `CLIReadyMessage` - CLI framework ready
- `CLICommandMessage` - CLI command execution
- `CLIStreamingMessage` - CLI streaming output
- `CLIErrorMessage` - CLI error handling

**Integration Points:**
- Command-line argument parsing
- Interactive prompt handling
- Progress indication
- Error display formatting

## Performance Characteristics

### Time Complexity
- **Enqueue**: O(log n) with priority queue, O(1) without priority
- **Dequeue**: O(log n) with priority queue, O(1) without priority  
- **Peek**: O(1) for next message
- **Size**: O(1) with counter tracking

### Space Complexity
- **Memory Usage**: O(n) where n is number of queued messages
- **Priority Overhead**: O(p) where p is number of priority levels
- **Statistics**: O(1) additional space for counters

### Throughput
- **Sequential Processing**: ~10K messages/second (Node.js single-thread)
- **Batch Operations**: Higher throughput for message batches
- **Memory Bounded**: Performance degrades gracefully under memory pressure

## Integration Patterns

### 1. CLI Framework Integration

```typescript
// CLI message processing loop
const processCommands = async (queue: IAsyncMessageQueue<QiMessage>) => {
  for await (const message of queue) {
    match(
      async (msg) => {
        switch (msg.type) {
          case MessageType.COMMAND:
            return await executeCommand(msg as CommandMessage)
          case MessageType.USER_INTERRUPT:
            return await handleInterrupt(msg as UserInterruptMessage)
          case MessageType.SYSTEM_ABORT:
            return await shutdown(msg as SystemAbortMessage)
        }
      },
      error => {
        console.error('Message processing error:', error.message)
        // Continue processing other messages
      },
      await queue.read()
    )
  }
}
```

### 2. Agent Communication

```typescript
// Agent output streaming
const streamAgentOutput = async (
  queue: IAsyncMessageQueue<QiMessage>, 
  agentId: string
) => {
  // Start streaming
  queue.enqueue(
    QiMessageFactory.createStreamStart(agentId),
    MessagePriority.HIGH
  )

  // Stream data chunks
  for (const chunk of agentOutputChunks) {
    queue.enqueue(
      QiMessageFactory.createStreamData(agentId, chunk),
      MessagePriority.NORMAL
    )
  }

  // End streaming
  queue.enqueue(
    QiMessageFactory.createStreamEnd(agentId),
    MessagePriority.HIGH
  )
}
```

### 3. Error Handling Integration

```typescript
// Comprehensive error handling
const handleQueueError = (error: QiError): void => {
  switch (error.category) {
    case 'VALIDATION':
      // Invalid message format
      console.error('Invalid message:', error.message)
      break
    case 'RESOURCE':
      // Queue capacity exceeded
      console.error('Queue full:', error.message)
      // Implement backpressure strategy
      break
    case 'SYSTEM':
      // Infrastructure failure
      console.error('System error:', error.message)
      // Implement retry with exponential backoff
      break
  }
}
```

## Quality Assurance

### Testing Strategy
- **Unit Tests**: Each component tested in isolation
- **Integration Tests**: Queue + message factory integration
- **Property Tests**: Message ordering and priority guarantees
- **Load Tests**: Performance under high message volume

### Error Handling Coverage
- **Message Validation**: Type-safe message creation and validation
- **Queue Capacity**: Graceful handling of queue overflow
- **Iterator Errors**: Proper error propagation through async iteration
- **Resource Cleanup**: Guaranteed cleanup on disposal

### Type Safety
- **Message Union Types**: Compile-time message type checking
- **Generic Queue**: Type-safe queue operations `QiAsyncMessageQueue<T>`
- **Proper Imports**: Explicit type imports for better tree-shaking
- **No Type Casting**: Eliminated all `as any` usage

## Usage Examples

### Basic Message Queue

```typescript
import { QiAsyncMessageQueue, QiMessageFactory, MessagePriority } from '@qi/amsg'

// Create and start queue
const queue = new QiAsyncMessageQueue({
  maxSize: 1000,
  priorityMode: true,
  autoStart: true,
  enableStats: true
})

// Enqueue messages with different priorities
const command = QiMessageFactory.createCommand('build', ['--production'])
queue.enqueue(command, MessagePriority.NORMAL)

const interrupt = QiMessageFactory.createUserInterrupt('Ctrl+C')
queue.enqueue(interrupt, MessagePriority.HIGH)

// Process messages (high priority first)
for await (const message of queue) {
  match(
    (msg) => {
      console.log(`Processing ${msg.type}:`, msg.id)
      // Handle message based on type
    },
    (error) => {
      console.error('Queue error:', error.message)
    },
    await queue.read()
  )
}
```

### CLI Integration

```typescript
// CLI command processing with message queue
const setupCLI = async () => {
  const queue = new QiAsyncMessageQueue<QiMessage>({
    priorityMode: true,
    onMessage: (msg) => console.log('Queued:', msg.type),
    onError: (error) => console.error('Queue error:', error.message)
  })

  // Start the queue
  match(
    () => console.log('CLI queue started'),
    (error) => {
      console.error('Failed to start queue:', error.message)
      process.exit(1)
    },
    queue.start()
  )

  return queue
}

// Command execution
const executeCommand = async (
  queue: IAsyncMessageQueue<QiMessage>,
  command: string,
  args: string[]
) => {
  const commandMessage = QiMessageFactory.createCommand(command, args)
  
  match(
    () => console.log('Command queued:', command),
    (error) => console.error('Failed to queue command:', error.message),
    queue.enqueue(commandMessage, MessagePriority.NORMAL)
  )
}
```

### Streaming Data Processing

```typescript
// Stream processing with message queue
const processStream = async (
  queue: IAsyncMessageQueue<QiMessage>,
  streamId: string
) => {
  // Listen for stream messages
  for await (const message of queue) {
    if (message.type.startsWith('STREAM_')) {
      const streamMsg = message as StreamMessage
      
      switch (streamMsg.type) {
        case MessageType.STREAM_START:
          console.log('Stream started:', streamMsg.streamId)
          break
        case MessageType.STREAM_DATA:
          console.log('Stream data:', streamMsg.data)
          break
        case MessageType.STREAM_END:
          console.log('Stream ended:', streamMsg.streamId)
          break
        case MessageType.STREAM_ERROR:
          console.error('Stream error:', streamMsg.error?.message)
          break
      }
    }
  }
}
```

## Anti-Pattern Elimination Results

The amsg module has been systematically cleaned:

✅ **Type Safety:**
- Replaced `{} as any` with proper `Record<MessageType, number>` types
- Added proper imports for all enum types
- Eliminated unsafe type assertions throughout

✅ **Functional Patterns:**
- All operations return `Result<T>` for composability
- Message factory functions use functional composition
- Error handling follows QiError patterns

✅ **Resource Management:**
- Proper cleanup functions with Result<T> returns
- No resource leaks in async operations
- Graceful shutdown with timeout handling

✅ **Code Quality:**
- Math.random usage structured with named constants
- Documented acceptable patterns (interface boundary throws)
- Consistent naming conventions throughout

The module now serves as a solid foundation for CLI and agent communication workflows while maintaining functional programming principles established in `lib/base`.