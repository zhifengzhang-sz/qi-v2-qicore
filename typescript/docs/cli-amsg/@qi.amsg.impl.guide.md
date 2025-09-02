# @qi/amsg Implementation Guide

## Overview

Complete implementation guide for the @qi/amsg package - an h2A-inspired async message queue system with QiCore Result<T> patterns for robust error handling and functional programming integration.

## Architecture Overview

### Core Components

```
@qi/amsg/
├── src/
│   ├── impl/                  # Core implementations
│   │   ├── QiAsyncMessageQueue.ts    # Main queue implementation
│   │   ├── QiMessageFactory.ts      # Message creation & validation
│   │   └── index.ts                 # Implementation exports
│   ├── interfaces/            # Interface definitions
│   │   ├── IAsyncMessageQueue.ts    # Queue interface
│   │   ├── IMessage.ts              # Message interfaces
│   │   └── index.ts                 # Interface exports
│   ├── types/                 # Type definitions
│   │   ├── MessageTypes.ts          # Message type system
│   │   ├── CLIMessageTypes.ts       # CLI-specific types
│   │   └── index.ts                 # Type exports
│   └── index.ts              # Main exports
```

### Key Architectural Patterns

1. **h2A Pattern**: Async iterator-based message processing inspired by Claude Code
2. **Result<T> Integration**: All operations return Result<T, QiError> for functional error handling
3. **Type-Safe Messages**: Comprehensive message type system with validation
4. **Non-blocking Operations**: Promise-based flow control with backpressure handling
5. **Correlation Patterns**: Request/response correlation with message chains

## 1. Message Type System

### 1.1 Core Message Types

#### Base Message Structure
```typescript
interface BaseMessage {
  readonly id: string;           // UUID v4
  readonly type: MessageType;    // Discriminated union
  readonly timestamp: Date;      // Creation time
  readonly priority: MessagePriority;
  readonly correlationId?: string;  // For request/response chains
  readonly parentId?: string;       // For message hierarchies
}
```

#### Message Type Enumeration
```typescript
enum MessageType {
  // User Interactions
  USER_INPUT = 'USER_INPUT',
  USER_INTERRUPT = 'USER_INTERRUPT',
  
  // Command System
  COMMAND = 'COMMAND',
  COMMAND_RESPONSE = 'COMMAND_RESPONSE',
  COMMAND_ERROR = 'COMMAND_ERROR',
  
  // Agent Communication
  AGENT_OUTPUT = 'AGENT_OUTPUT',
  AGENT_ERROR = 'AGENT_ERROR',
  AGENT_THINKING = 'AGENT_THINKING',
  
  // Streaming Operations
  STREAM_START = 'STREAM_START',
  STREAM_DATA = 'STREAM_DATA',
  STREAM_END = 'STREAM_END',
  STREAM_ERROR = 'STREAM_ERROR',
  
  // System Control
  SYSTEM_CONTROL = 'SYSTEM_CONTROL',
  SYSTEM_STATUS = 'SYSTEM_STATUS',
  SYSTEM_ABORT = 'SYSTEM_ABORT'
}
```

#### Priority System
```typescript
enum MessagePriority {
  CRITICAL = 0,  // System abort, user interrupts
  HIGH = 1,      // Errors, system control
  NORMAL = 2,    // Regular operations
  LOW = 3        // Background tasks, status updates
}
```

### 1.2 Specific Message Types

#### User Input Message
```typescript
interface UserInputMessage extends BaseMessage {
  type: MessageType.USER_INPUT;
  input: string;                    // Raw user input
  source: 'stdin' | 'cli' | 'api'; // Input source
  raw: boolean;                     // Is raw input (no processing)
}
```

#### Command Message
```typescript
interface CommandMessage extends BaseMessage {
  type: MessageType.COMMAND;
  command: string;              // Command name
  args: readonly any[];         // Command arguments
  context: CommandContext;      // Execution context
}

interface CommandContext {
  executionId: string;          // Unique execution ID
  userId?: string;              // User identifier
  sessionId?: string;           // Session identifier
  environment?: Record<string, unknown>; // Environment variables
}
```

#### Agent Output Message
```typescript
interface AgentOutputMessage extends BaseMessage {
  type: MessageType.AGENT_OUTPUT;
  content: string;                          // Agent response content
  format: 'text' | 'markdown' | 'json';   // Content format
  streaming: boolean;                       // Is part of streaming response
  metadata?: {
    model?: string;                         // LLM model used
    tokens?: number;                        // Token count
    executionTime?: number;                 // Processing time
  };
}
```

#### System Control Message
```typescript
interface SystemControlMessage extends BaseMessage {
  type: MessageType.SYSTEM_CONTROL;
  action: 'pause' | 'resume' | 'reset' | 'shutdown';
  immediate: boolean;           // Should execute immediately
  reason?: string;              // Reason for control action
}
```

## 2. Message Factory Implementation

### 2.1 QiMessageFactory Class

**Purpose**: Type-safe message creation with automatic validation and ID generation

#### Core Features
- Automatic UUID generation for message IDs
- Type-specific validation
- Priority inference based on message type
- Correlation ID management
- Result<T> error handling for all operations

#### Implementation Structure
```typescript
class QiMessageFactory implements IMessageFactory {
  // Primary creation method
  createMessage<T extends QiMessage>(
    type: T['type'],
    payload: Omit<T, keyof BaseMessage>
  ): Result<T, FactoryError>
  
  // Specialized creation methods
  createUserInputMessage(
    input: string,
    source: 'stdin' | 'cli' | 'api',
    raw?: boolean
  ): Result<UserInputMessage, FactoryError>
  
  createCommandMessage(
    command: string,
    args: readonly any[],
    context: CommandContext
  ): Result<CommandMessage, FactoryError>
  
  createAgentOutputMessage(
    content: string,
    format?: 'text' | 'markdown' | 'json',
    streaming?: boolean
  ): Result<AgentOutputMessage, FactoryError>
  
  createSystemControlMessage(
    action: 'pause' | 'resume' | 'reset' | 'shutdown',
    immediate?: boolean,
    reason?: string
  ): Result<SystemControlMessage, FactoryError>
  
  // Correlation methods
  createCorrelatedMessage<T extends QiMessage>(
    type: T['type'],
    payload: Omit<T, keyof BaseMessage>,
    parentMessage: QiMessage
  ): Result<T, FactoryError>
  
  createResponseMessage(
    commandMessage: CommandMessage,
    result: any,
    executionTime: number
  ): Result<QiMessage, FactoryError>
}
```

### 2.2 Validation System

#### Message Validation Rules
```typescript
// Base validation for all messages
- Message ID must be present (UUID v4)
- Message type must be valid enum value
- Timestamp must be valid Date
- Priority must be valid enum value

// Type-specific validation
CommandMessage:
  - command must be non-empty string
  - args must be array
  - context.executionId must be present

UserInputMessage:
  - input must be string
  - source must be valid enum value

AgentOutputMessage:
  - content must be string
  - format must be valid enum value
  - if format is 'json', content must be valid JSON
```

#### Error Handling Pattern
```typescript
// All validation errors use QiCore Result<T> pattern
interface FactoryError extends QiError {
  context: {
    operation?: string;
    messageType?: string;
    validation?: string[];
  };
}

// Usage pattern
const result = messageFactory.createUserInputMessage("hello", "cli");
return match(
  (message) => processMessage(message),
  (error) => handleError(error),
  result
);
```

## 3. Async Message Queue Implementation

### 3.1 QiAsyncMessageQueue Class

**Purpose**: High-performance async message queue with backpressure handling, priority queuing, and h2A-inspired iteration patterns.

#### Core Architecture
```typescript
class QiAsyncMessageQueue implements IAsyncMessageQueue {
  private queues: Map<MessagePriority, QiMessage[]>;
  private processors: Set<(message: QiMessage) => Promise<void>>;
  private subscribers: Set<QueueSubscriber>;
  private state: QueueState;
  private options: QueueOptions;
  
  // Core queue operations
  enqueue(message: QiMessage): Promise<Result<void, QueueError>>
  dequeue(): Promise<Result<QiMessage | null, QueueError>>
  peek(): Result<QiMessage | null, QueueError>
  size(): number
  
  // h2A-inspired async iteration
  [Symbol.asyncIterator](): AsyncIterator<QiMessage>
  
  // Lifecycle management
  start(): Promise<Result<void, QueueError>>
  stop(): Promise<Result<void, QueueError>>
  pause(): Promise<Result<void, QueueError>>
  resume(): Promise<Result<void, QueueError>>
  
  // Subscription system
  subscribe(callback: QueueSubscriber): () => void
  
  // Statistics and monitoring
  getStats(): MessageStats
  getState(): QueueState
}
```

### 3.2 Priority Queue Implementation

#### Priority Processing Logic
```typescript
// Messages processed in strict priority order
CRITICAL (0) → HIGH (1) → NORMAL (2) → LOW (3)

// Within same priority, FIFO order
// Critical messages can interrupt processing of lower priority
```

#### Queue State Management
```typescript
enum QueueState {
  STOPPED = 'stopped',      // Queue not processing
  STARTING = 'starting',    // Queue initializing
  RUNNING = 'running',      // Normal operation
  PAUSING = 'pausing',      // Graceful pause in progress
  PAUSED = 'paused',        // Queue paused
  STOPPING = 'stopping',    // Graceful stop in progress
  ERROR = 'error'           // Error state
}
```

### 3.3 h2A-Inspired Async Iteration

#### Async Iterator Pattern
```typescript
// h2A pattern - async iteration over messages
async *[Symbol.asyncIterator](): AsyncIterator<QiMessage> {
  while (this.state === QueueState.RUNNING) {
    const result = await this.dequeue();
    
    yield* match(
      function* (message: QiMessage) {
        if (message) yield message;
      },
      function* (error: QueueError) {
        // Handle error, potentially yield error message
        console.error('Queue iteration error:', error);
      },
      result
    );
    
    // Yield control to event loop
    await new Promise(resolve => setImmediate(resolve));
  }
}
```

#### Usage Pattern
```typescript
// Consumer pattern
for await (const message of messageQueue) {
  const processingResult = await processMessage(message);
  
  match(
    () => console.log('Message processed successfully'),
    (error) => console.error('Processing error:', error),
    processingResult
  );
}
```

### 3.4 Backpressure Management

#### Configuration Options
```typescript
interface QueueOptions {
  maxSize: number;                    // Maximum queue size
  maxConcurrentProcessing: number;    // Max concurrent message processing
  pauseThreshold: number;             // Auto-pause threshold
  resumeThreshold: number;            // Auto-resume threshold
  processingTimeout: number;          // Message processing timeout
  retryAttempts: number;              // Retry failed messages
  retryDelay: number;                 // Delay between retries
}
```

#### Backpressure Strategies
```typescript
// When queue reaches maxSize
1. Reject new messages (fail-fast)
2. Drop lowest priority messages
3. Pause producers
4. Apply exponential backoff

// Implementation
async enqueue(message: QiMessage): Promise<Result<void, QueueError>> {
  if (this.size() >= this.options.maxSize) {
    if (message.priority === MessagePriority.CRITICAL) {
      // Critical messages always accepted
      this.makeRoom();
    } else {
      return failure(queueError.queueFull());
    }
  }
  
  return this.addToQueue(message);
}
```

## 4. Queue Interfaces and Contracts

### 4.1 Core Interfaces

#### IAsyncMessageQueue Interface
```typescript
interface IAsyncMessageQueue {
  // Queue operations
  enqueue(message: QiMessage): Promise<Result<void, QueueError>>;
  dequeue(): Promise<Result<QiMessage | null, QueueError>>;
  peek(): Result<QiMessage | null, QueueError>;
  size(): number;
  
  // Async iteration
  [Symbol.asyncIterator](): AsyncIterator<QiMessage>;
  
  // Lifecycle
  start(): Promise<Result<void, QueueError>>;
  stop(): Promise<Result<void, QueueError>>;
  pause(): Promise<Result<void, QueueError>>;
  resume(): Promise<Result<void, QueueError>>;
  
  // Monitoring
  subscribe(callback: QueueSubscriber): () => void;
  getStats(): MessageStats;
  getState(): QueueState;
}
```

#### Message Factory Interface
```typescript
interface IMessageFactory {
  createMessage<T extends QiMessage>(
    type: T['type'],
    payload: Omit<T, keyof BaseMessage>
  ): Result<T, FactoryError>;
  
  createCorrelatedMessage<T extends QiMessage>(
    type: T['type'],
    payload: Omit<T, keyof BaseMessage>,
    parentMessage: QiMessage
  ): Result<T, FactoryError>;
}
```

### 4.2 Subscription System

#### Queue Subscriber Pattern
```typescript
type QueueSubscriber = (event: QueueEvent) => void;

interface QueueEvent {
  type: QueueEventType;
  message?: QiMessage;
  error?: QueueError;
  stats?: MessageStats;
}

enum QueueEventType {
  MESSAGE_ENQUEUED = 'message_enqueued',
  MESSAGE_DEQUEUED = 'message_dequeued',
  MESSAGE_PROCESSED = 'message_processed',
  MESSAGE_FAILED = 'message_failed',
  QUEUE_PAUSED = 'queue_paused',
  QUEUE_RESUMED = 'queue_resumed',
  QUEUE_ERROR = 'queue_error'
}
```

## 5. Error Handling Strategy

### 5.1 Error Types

#### Queue-Specific Errors
```typescript
interface QueueError extends QiError {
  context: {
    operation?: string;
    queueSize?: number;
    messageId?: string;
    messageType?: string;
    timestamp?: string;
    queueLength?: number;
  };
}

// Error factories
const queueError = {
  queueFull: () => create('QUEUE_FULL', 'Message queue is full'),
  invalidMessage: (id: string) => create('INVALID_MESSAGE', `Invalid message: ${id}`),
  processingTimeout: (id: string) => create('TIMEOUT', `Message processing timeout: ${id}`),
  queueStopped: () => create('QUEUE_STOPPED', 'Queue is not running'),
};
```

### 5.2 Error Recovery Patterns

#### Retry Strategy
```typescript
// Exponential backoff for failed messages
class MessageRetryHandler {
  async retryMessage(
    message: QiMessage, 
    error: QueueError, 
    attempt: number
  ): Promise<Result<void, QueueError>> {
    if (attempt >= this.maxRetries) {
      return failure(queueError.maxRetriesExceeded(message.id));
    }
    
    const delay = Math.pow(2, attempt) * this.baseDelay;
    await this.wait(delay);
    
    return this.processMessage(message);
  }
}
```

## 6. Performance and Monitoring

### 6.1 Performance Metrics

#### Message Statistics
```typescript
interface MessageStats {
  totalMessages: number;
  messagesPerSecond: number;
  averageProcessingTime: number;
  queueLength: number;
  failedMessages: number;
  retriedMessages: number;
  memoryUsage: number;
  
  // Per-priority breakdown
  priorityBreakdown: {
    [key in MessagePriority]: {
      count: number;
      averageProcessingTime: number;
      failureRate: number;
    };
  };
}
```

### 6.2 Debug and Logging

#### Debug Logger Integration
```typescript
// Built-in debug logging
const logger = createDebugLogger('QiAsyncMessageQueue');

// Log levels
logger.debug('Message enqueued', { messageId, type, priority });
logger.info('Queue state changed', { oldState, newState });
logger.warn('Queue approaching capacity', { currentSize, maxSize });
logger.error('Message processing failed', { messageId, error });
```

## 7. Integration Patterns

### 7.1 CLI Integration

#### Message Flow in CLI
```typescript
// CLI → Message Queue → Agent
class MessageDrivenCLI {
  constructor(private messageQueue: QiAsyncMessageQueue) {}
  
  async processUserInput(input: string): Promise<void> {
    if (input.startsWith('/')) {
      // Handle CLI commands locally
      this.handleCLICommand(input);
    } else {
      // Send to agent via message queue
      const message = messageFactory.createUserInputMessage(input, 'cli');
      await this.messageQueue.enqueue(message);
    }
  }
}
```

### 7.2 Agent Integration

#### Agent Message Processing
```typescript
// Agent → Message Queue → CLI
class AgentMessageProcessor {
  constructor(private messageQueue: QiAsyncMessageQueue) {}
  
  async initialize(): Promise<void> {
    // Process messages from queue
    for await (const message of this.messageQueue) {
      await this.handleMessage(message);
    }
  }
  
  private async handleMessage(message: QiMessage): Promise<void> {
    match(
      async () => {
        switch (message.type) {
          case MessageType.USER_INPUT:
            return this.processUserInput(message as UserInputMessage);
          case MessageType.COMMAND:
            return this.processCommand(message as CommandMessage);
          default:
            return this.handleUnknownMessage(message);
        }
      },
      (error) => this.handleError(error, message),
      await this.validateMessage(message)
    );
  }
}
```

## 8. Implementation Priorities

### Phase 1: Core Infrastructure ✅
1. Message type system and validation
2. QiMessageFactory implementation
3. Basic QiAsyncMessageQueue
4. Result<T> error handling integration

### Phase 2: Advanced Queue Features
1. **Priority queue implementation**
2. **Async iterator (h2A pattern)**
3. **Backpressure management**
4. **Subscription system**

### Phase 3: Performance & Monitoring
1. **Statistics collection**
2. **Debug logging integration**
3. **Performance optimization**
4. **Memory management**

### Phase 4: Integration & Testing
1. **CLI integration patterns**
2. **Agent integration patterns**
3. **Comprehensive testing**
4. **Documentation and examples**

## 9. Testing Strategy

### 9.1 Unit Tests
- Message creation and validation
- Queue operations (enqueue/dequeue)
- Priority ordering
- Error handling paths
- Statistics collection

### 9.2 Integration Tests
- CLI → Queue → Agent message flow
- Backpressure scenarios
- Error recovery patterns
- Performance under load

### 9.3 Performance Tests
- Message throughput benchmarks
- Memory usage under load
- Priority queue performance
- Async iteration efficiency

## 10. Implementation Notes

### Critical Success Factors

1. **h2A Pattern Fidelity**: Proper async iterator implementation
2. **Result<T> Integration**: All operations must return Result<T, Error>
3. **Type Safety**: Comprehensive message type system
4. **Performance**: Handle high message volumes efficiently
5. **Error Recovery**: Robust error handling and retry logic

### Common Pitfalls to Avoid

1. **Memory Leaks**: Proper cleanup of subscribers and processors
2. **Blocking Operations**: All I/O must be async
3. **Priority Inversion**: Ensure critical messages are processed first
4. **Error Swallowing**: All errors must be handled via Result<T>
5. **Queue Starvation**: Balance between priorities

This implementation guide provides the complete architecture and implementation details needed to build the @qi/amsg package in the QiCore ecosystem with full h2A pattern support and robust error handling.