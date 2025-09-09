# @qi/cli API Reference

## Overview

The @qi/cli package provides a message-driven CLI framework with hotkey support, progress display, and agent integration using QiCore Result<T> patterns for robust error handling.

## Installation

```bash
bun add @qi/cli
```

## Quick Start

```typescript
import { createCLIAsync, MessageDrivenCLI, CLIStateManager } from '@qi/cli';
import { QiAsyncMessageQueue } from '@qi/amsg';
import { type Result, match } from '@qi/base';

// Create message queue and state manager
const messageQueue = new QiAsyncMessageQueue({ maxSize: 1000 });
const stateManager = new CLIStateManager('session-' + Date.now());

// Create CLI instance
const cli = await createCLIAsync({
  messageQueue,
  stateManager,
  config: {
    framework: 'hybrid',
    enableHotkeys: true,
    enableModeIndicator: true
  }
});

// Start the CLI
const startResult = await cli.start();
match(
  () => console.log('CLI started successfully'),
  (error) => console.error('Failed to start CLI:', error),
  startResult
);
```

## Core API

### MessageDrivenCLI

The main CLI class that orchestrates all CLI operations through message passing.

#### Constructor

```typescript
class MessageDrivenCLI {
  constructor(dependencies: CLIDependencies)
}

interface CLIDependencies {
  messageQueue: QiAsyncMessageQueue;
  stateManager: IStateManager;
  commandHandler: ICommandHandler;
  keyboardManager?: IKeyboardManager;
  uiRenderer?: IUIRenderer;
}
```

#### Methods

##### `initialize(): Promise<Result<void, InitializationError>>`

Initialize all CLI components and dependencies.

```typescript
const initResult = await cli.initialize();
match(
  () => console.log('Initialized successfully'),
  (error) => console.error('Initialization failed:', error),
  initResult
);
```

##### `start(): Promise<Result<void, StartupError>>`

Start the CLI and begin message processing.

```typescript
const startResult = await cli.start();
if (startResult.tag === 'failure') {
  console.error('CLI startup failed:', startResult.error);
}
```

##### `stop(): Promise<Result<void, ShutdownError>>`

Stop the CLI and cleanup resources.

```typescript
await cli.stop();
```

##### `getStatus(): CLIStatus`

Get current CLI status information.

```typescript
const status = cli.getStatus();
console.log(`CLI running: ${status.isRunning}`);
console.log(`Current state: ${status.currentState}`);
```

### Factory Functions

#### `createCLIAsync(options: CLICreationOptions): Promise<MessageDrivenCLI>`

Create a new CLI instance with automatic dependency injection.

```typescript
interface CLICreationOptions {
  messageQueue: QiAsyncMessageQueue;
  stateManager: IStateManager;
  commandHandler?: ICommandHandler;
  config: CLIConfig;
}

const cli = await createCLIAsync({
  messageQueue: new QiAsyncMessageQueue({ maxSize: 1000 }),
  stateManager: new CLIStateManager(),
  config: {
    framework: 'hybrid',
    enableHotkeys: true,
    enableModeIndicator: true
  }
});
```

#### `createReadlineCLI(config: ReadlineConfig): Promise<ReadlineCLI>`

Create a readline-based CLI for simple use cases.

```typescript
const readlineCLI = await createReadlineCLI({
  prompt: '> ',
  historySize: 100,
  enableAutoComplete: true
});
```

### State Management

#### CLIStateManager

Manages CLI application state with XState 5 integration.

```typescript
class CLIStateManager implements IStateManager {
  constructor(sessionId?: string)
  
  // State queries
  getCurrentState(): AppState;
  getCurrentSubState(): AppSubState;
  getStateContext(): AppStateContext;
  
  // State mutations
  setBusy(taskName: string): void;
  setReady(subState?: AppSubState): void;
  cycleReadyStates(): void;
  
  // Subscription
  subscribe(callback: StateCallback): () => void;
}
```

##### States

- **`ready`**: CLI is ready for user interaction
  - `generic`: General chat mode
  - `planning`: Strategic planning mode
  - `editing`: Code editing mode
- **`busy`**: CLI is processing a task

##### Usage

```typescript
const stateManager = new CLIStateManager('my-session');

// Subscribe to state changes
const unsubscribe = stateManager.subscribe((state) => {
  console.log(`State changed: ${state.currentState}.${state.currentSubState}`);
});

// Change states
stateManager.setBusy('processing user request');
stateManager.setReady('planning'); // Set to ready.planning
stateManager.cycleReadyStates(); // Cycle: planning → editing → generic
```

### Command System

#### ICommandHandler

Interface for handling CLI commands (commands starting with `/`).

```typescript
interface ICommandHandler {
  isCommand(input: string): boolean;
  parseCommand(input: string): CLICommandRequest | null;
  executeCommand(request: CLICommandRequest): Promise<CLICommandResult>;
  getCommands(): CLICommand[];
}
```

#### SimpleCLICommandHandler

Built-in command handler with common commands.

```typescript
const commandHandler = new SimpleCLICommandHandler();

// Built-in commands
// /help - Show available commands
// /clear - Clear screen  
// /history - Show command history
// /status - Show CLI status
// /quit or /exit - Exit CLI
```

#### Custom Commands

```typescript
const customHandler = new SimpleCLICommandHandler();

// Register custom command
customHandler.registerCommand({
  name: 'greet',
  description: 'Greet the user',
  usage: '/greet [name]',
  handler: async (request) => {
    const name = request.args[0] || 'there';
    return success(`Hello, ${name}!`);
  }
});
```

### Keyboard Management

#### HotkeyManager

Handles keyboard shortcuts and special key combinations.

```typescript
interface HotkeyConfig {
  enableShiftTab: boolean;
  enableEscape: boolean;
  enableCtrlC: boolean;
  customHotkeys?: HotkeyDefinition[];
}

const hotkeyManager = new HotkeyManager(messageQueue, {
  enableShiftTab: true,  // Cycle CLI states
  enableEscape: true,    // Cancel current operation
  enableCtrlC: true,     // Interrupt processing
  customHotkeys: [
    {
      key: 'F1',
      action: () => commandHandler.executeCommand({ command: 'help', args: [] })
    }
  ]
});
```

#### Built-in Hotkeys

- **Shift+Tab**: Cycle through ready states (planning → editing → generic)
- **Escape**: Cancel current operation or clear input
- **Ctrl+C**: Interrupt processing and return to ready state

### UI Components

#### ModeIndicator

Visual indicator of current CLI state and mode.

```typescript
const modeIndicator = new ModeIndicator({
  theme: 'default',
  enableAnimations: true,
  updateInterval: 100
});

// Get mode display information
const modeInfo = getModeColor('ready.planning');
console.log(`${getModeEmoji('ready.planning')} ${modeInfo.label}`);
```

#### ProgressDisplay

Shows progress for long-running operations.

```typescript
const progressDisplay = new ProgressDisplay({
  enableSpinners: true,
  enableProgressBars: true,
  theme: 'default'
});

// Show progress
progressDisplay.showProgress('processing', 0.3, 'Analyzing code...');
progressDisplay.showSpinner('waiting', 'Waiting for response...');
```

#### StreamingRenderer

Renders streaming text output from agents.

```typescript
const streamingRenderer = new StreamingRenderer({
  wrapWidth: 80,
  enableWordWrap: true,
  throttleDelay: 16 // ~60fps
});

// Stream content
streamingRenderer.startStream();
streamingRenderer.addChunk('Hello ');
streamingRenderer.addChunk('world!');
streamingRenderer.endStream();
```

### Configuration

#### CLIConfig

Main configuration interface for CLI behavior.

```typescript
interface CLIConfig {
  // Framework selection
  framework?: 'readline' | 'ink' | 'hybrid';
  
  // Feature flags
  enableHotkeys: boolean;
  enableModeIndicator: boolean;
  enableProgressDisplay: boolean;
  enableStreaming: boolean;
  
  // Appearance
  prompt: string;
  colors: boolean;
  animations: boolean;
  
  // Input handling
  historySize: number;
  autoComplete: boolean;
  
  // Performance
  streamingThrottle: number; // ms delay between chunks
  maxBufferSize: number;
  
  // Debugging
  debug: boolean;
}
```

#### Default Configuration

```typescript
export const DefaultCLIConfig: CLIConfig = {
  enableHotkeys: true,
  enableModeIndicator: true,
  enableProgressDisplay: true,
  enableStreaming: true,
  prompt: '> ',
  colors: true,
  animations: true,
  historySize: 100,
  autoComplete: false,
  streamingThrottle: 0,
  maxBufferSize: 10000,
  debug: false,
};
```

### Framework Selection

#### Available Frameworks

- **`readline`**: Terminal-based interface using Node.js readline
- **`ink`**: React-based terminal UI (requires React)
- **`hybrid`**: Automatic selection based on environment

#### Automatic Detection

```typescript
import { autoDetectFramework, recommendFramework } from '@qi/cli';

// Detect available frameworks
const available = getAvailableFrameworks();
console.log('Available frameworks:', available);

// Get recommendation
const recommended = recommendFramework();
console.log('Recommended framework:', recommended);

// Auto-detect and create
const config = autoDetectFramework();
const cli = await createCLIAsync({
  messageQueue: new QiAsyncMessageQueue(),
  stateManager: new CLIStateManager(),
  config
});
```

### Error Handling

All CLI operations return `Result<T, QiError>` for functional error handling.

#### Common Error Types

```typescript
// Initialization errors
interface InitializationError extends QiError {
  context: {
    component?: string;
    operation?: string;
    dependencies?: string[];
  };
}

// Startup errors
interface StartupError extends QiError {
  context: {
    framework?: string;
    port?: number;
    timeout?: number;
  };
}

// Command errors
interface CommandError extends QiError {
  context: {
    command?: string;
    args?: any[];
    validation?: string[];
  };
}
```

#### Error Handling Patterns

```typescript
// Simple error handling
const result = await cli.start();
if (result.tag === 'failure') {
  console.error('CLI failed:', result.error.message);
  process.exit(1);
}

// Functional error handling
const result = await cli.executeCommand('/help');
match(
  (output) => console.log('Command output:', output),
  (error) => {
    console.error('Command failed:', error.message);
    // Log full error context for debugging
    console.debug('Error context:', error.context);
  },
  result
);
```

### Integration with @qi/amsg

The CLI integrates seamlessly with the @qi/amsg message queue system.

#### Message Flow

```
User Input → CLI → Message Queue → Agent → Response → CLI → Display
```

#### Usage Pattern

```typescript
import { QiAsyncMessageQueue, MessageType } from '@qi/amsg';
import { createCLIAsync } from '@qi/cli';

// Create message queue
const messageQueue = new QiAsyncMessageQueue({
  maxSize: 1000,
  maxConcurrentProcessing: 5,
  priorityQueuing: true
});

// Create CLI with message queue
const cli = await createCLIAsync({
  messageQueue,
  stateManager: new CLIStateManager(),
  config: { framework: 'hybrid' }
});

// Process messages from queue
for await (const message of messageQueue) {
  if (message.type === MessageType.AGENT_OUTPUT) {
    console.log('Agent response:', message.content);
  }
}
```

### Advanced Usage

#### Custom Framework Implementation

```typescript
class CustomCLIFramework implements ICLIFramework {
  async initialize(): Promise<Result<void, InitializationError>> {
    // Initialize custom framework
    return success(undefined);
  }
  
  async start(): Promise<Result<void, StartupError>> {
    // Start custom framework
    return success(undefined);
  }
  
  // Implement other required methods...
}
```

#### Dependency Injection

```typescript
// Custom dependency injection
const customDependencies: CLIDependencies = {
  messageQueue: new QiAsyncMessageQueue(),
  stateManager: new CLIStateManager(),
  commandHandler: new CustomCommandHandler(),
  keyboardManager: new CustomKeyboardManager(),
  uiRenderer: new CustomUIRenderer()
};

const cli = new MessageDrivenCLI(customDependencies);
```

### Testing

#### Unit Testing

```typescript
import { describe, it, expect } from 'vitest';
import { CLIStateManager } from '@qi/cli';

describe('CLIStateManager', () => {
  it('should cycle through ready states', () => {
    const stateManager = new CLIStateManager();
    
    expect(stateManager.getCurrentSubState()).toBe('generic');
    
    stateManager.cycleReadyStates();
    expect(stateManager.getCurrentSubState()).toBe('planning');
    
    stateManager.cycleReadyStates();
    expect(stateManager.getCurrentSubState()).toBe('editing');
  });
});
```

#### Integration Testing

```typescript
describe('CLI Integration', () => {
  it('should process user input through message queue', async () => {
    const messageQueue = new QiAsyncMessageQueue();
    const cli = await createCLIAsync({
      messageQueue,
      stateManager: new CLIStateManager(),
      config: { framework: 'readline' }
    });
    
    // Test message flow
    await cli.processInput('Hello, agent!');
    
    // Verify message was queued
    const message = await messageQueue.dequeue();
    expect(message.tag).toBe('success');
    expect(message.value?.type).toBe('USER_INPUT');
  });
});
```

## Examples

### Basic CLI Application

```typescript
import { createCLIAsync, CLIStateManager, SimpleCLICommandHandler } from '@qi/cli';
import { QiAsyncMessageQueue } from '@qi/amsg';

async function main() {
  const messageQueue = new QiAsyncMessageQueue({ maxSize: 100 });
  const stateManager = new CLIStateManager();
  const commandHandler = new SimpleCLICommandHandler();
  
  const cli = await createCLIAsync({
    messageQueue,
    stateManager,
    commandHandler,
    config: {
      framework: 'hybrid',
      enableHotkeys: true,
      enableModeIndicator: true,
      prompt: 'qi> '
    }
  });
  
  await cli.start();
}

main().catch(console.error);
```

### Agent Integration

```typescript
import { createCLIAsync, CLIStateManager } from '@qi/cli';
import { QiAsyncMessageQueue, MessageType } from '@qi/amsg';

class AgentProcessor {
  constructor(private messageQueue: QiAsyncMessageQueue) {}
  
  async start() {
    for await (const message of this.messageQueue) {
      if (message.type === MessageType.USER_INPUT) {
        // Process with agent
        const response = await this.processWithAgent(message.input);
        
        // Send response back
        const responseMessage = messageFactory.createAgentOutputMessage(
          response, 'text', false
        );
        await this.messageQueue.enqueue(responseMessage.value);
      }
    }
  }
  
  private async processWithAgent(input: string): Promise<string> {
    // Your agent processing logic here
    return `Agent response to: ${input}`;
  }
}

async function main() {
  const messageQueue = new QiAsyncMessageQueue();
  const cli = await createCLIAsync({
    messageQueue,
    stateManager: new CLIStateManager(),
    config: { framework: 'hybrid' }
  });
  
  // Start agent processor
  const agent = new AgentProcessor(messageQueue);
  agent.start();
  
  // Start CLI
  await cli.start();
}
```

## API Summary

### Core Classes

| Class | Purpose | Key Methods |
|-------|---------|-------------|
| `MessageDrivenCLI` | Main CLI orchestrator | `start()`, `stop()`, `getStatus()` |
| `CLIStateManager` | State management | `setBusy()`, `setReady()`, `cycleReadyStates()` |
| `SimpleCLICommandHandler` | Command processing | `executeCommand()`, `registerCommand()` |
| `HotkeyManager` | Keyboard shortcuts | Key event handling |

### UI Components

| Component | Purpose | Key Features |
|-----------|---------|--------------|
| `ModeIndicator` | State visualization | Emoji + color coding |
| `ProgressDisplay` | Progress feedback | Spinners, progress bars |
| `StreamingRenderer` | Text streaming | Word wrapping, throttling |

### Factory Functions

| Function | Purpose | Returns |
|----------|---------|---------|
| `createCLIAsync()` | Create full-featured CLI | `MessageDrivenCLI` |
| `createReadlineCLI()` | Create simple CLI | `ReadlineCLI` |
| `autoDetectFramework()` | Detect best framework | `CLIConfig` |

This API reference covers the complete @qi/cli package interface for building robust, message-driven CLI applications with the QiCore ecosystem.