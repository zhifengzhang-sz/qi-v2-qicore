# CLI-AMSG Integration Example

This example demonstrates the proper integration patterns between `@qi/cli` and `@qi/amsg` following the QiCore architectural principles.

## What It Demonstrates

### 1. Message-Driven Architecture
- **CLI → AMSG → Agent flow**: All communication happens through the message queue
- **No direct function calls**: CLI never calls agent directly, everything goes through messages
- **Pure message-driven**: Follows h2A patterns with async iteration

### 2. Priority Queue Management
- **Priority-based processing**: Critical > High > Normal > Low
- **Backpressure handling**: Drops lower priority messages when queue is full
- **Critical message override**: Critical messages can preempt lower priority ones

### 3. QiCore Result<T> Patterns
- **All operations return Result<T>**: No exceptions thrown, explicit error handling
- **Functional composition**: Uses `map`, `flatMap`, `match` for error handling
- **Error categorization**: Proper error types (VALIDATION, SYSTEM, etc.)

### 4. Subscription-Based Monitoring
- **Queue event subscription**: Monitor message flow in real-time
- **Statistics tracking**: Message counts, processing times, error rates
- **Priority breakdown**: See queue status by priority level

### 5. State Management Integration
- **XState compatibility**: Designed to work with CLI state manager
- **Message correlation**: Request/response tracking with correlation IDs
- **Lifecycle management**: Proper startup and shutdown sequences

## Running the Example

```bash
# From the typescript directory
bun run app/cli-amsg-example/src/index.ts

# Or from the example directory
cd app/cli-amsg-example
bun run dev
```

## Expected Output

```
🚀 QiCore CLI-AMSG Integration Example
=====================================

🤖 Mock Agent started - processing messages...

💻 Mock CLI started

📋 Demonstrating message flow patterns:

📥 Queue: Message enqueued (user_input, Priority: 2)
📤 Queue: Message dequeued (user_input)
📨 Agent processing: user_input (ID: 12345678, Priority: 2)
💬 User said: "Hello!"
🤖 Agent response: "👋 Hello! How can I help you today?"
...
```

## Architecture Patterns

### Message Flow
```
┌─────────┐    ┌──────────────┐    ┌───────────┐
│   CLI   │───▶│ QiAsyncQueue │───▶│   Agent   │
└─────────┘    └──────────────┘    └───────────┘
     ▲              │                     │
     └──────────────┴─────────────────────┘
           Response messages flow back
```

### Error Handling Pattern
```typescript
// Every operation returns Result<T>
const result = await someOperation()

return match(
  (value) => handleSuccess(value),
  (error) => handleError(error), 
  result
)
```

### Priority Handling
```typescript
// Messages processed by priority
CRITICAL (0) → HIGH (1) → NORMAL (2) → LOW (3)

// Critical messages can override queue limits
if (message.priority === MessagePriority.CRITICAL) {
  makeRoomForCriticalMessage()
}
```

## Key Implementation Notes

### 1. Pure Message Communication
- CLI never calls agent methods directly
- All communication through `messageQueue.enqueue()`
- Agent processes messages via async iteration

### 2. Proper Result<T> Usage
- All operations return `Result<Success, QiError>`
- Use `match()` for handling both success and failure cases
- Chain operations with `flatMap()` for composition

### 3. Priority Queue Benefits
- Critical system messages always processed first
- Graceful degradation under load
- Backpressure prevents system overload

### 4. Subscription Pattern
- Monitor queue events for debugging/metrics
- Non-blocking event notifications
- Easy to add logging, analytics, etc.

## Integration with Real CLI

When integrated with the full `@qi/cli` framework:

```typescript
// Real integration would look like:
import { HybridFramework } from '@qi/cli/frameworks/hybrid'
import { CLIStateManager } from '@qi/cli/impl/state-manager' 
import { HotkeyManager } from '@qi/cli/keyboard/HotkeyManager'

const cli = new HybridFramework(config, messageQueue)
const stateManager = new CLIStateManager()
const hotkeyManager = new HotkeyManager(messageQueue)
```

## Performance Characteristics

- **O(1) priority insertion**: Separate queues per priority
- **Memory efficient**: TTL-based message expiration
- **Backpressure aware**: Configurable limits and drop policies
- **Non-blocking**: All operations are async with proper flow control

This example provides a complete foundation for building sophisticated CLI applications with proper message-driven architecture and QiCore patterns.