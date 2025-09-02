# @qi/cli Implementation Guide

## Overview

Complete implementation guide for the @qi/cli package - a sophisticated CLI framework with state management, command parsing, and message-driven architecture extracted from qi-v2-agent.

## Architecture Overview

### Core Components

```
@qi/cli/
├── src/
│   ├── abstractions/          # Interfaces and type definitions
│   ├── impl/                  # Core implementations
│   ├── frameworks/            # UI framework adapters (Ink, Readline, Hybrid)
│   ├── keyboard/              # Keyboard input handling
│   ├── ui/                    # UI components (Progress, Mode indicators)
│   ├── factories/             # Factory functions
│   └── index.ts              # Main exports
```

### Key Architectural Patterns

1. **Message-Driven Architecture**: Pure message queue communication via @qi/amsg
2. **State Machine**: XState 5-based hierarchical state management
3. **Command Separation**: CLI commands vs Agent commands
4. **Framework Agnostic**: Pluggable UI frameworks (Readline/Ink/Hybrid)

## 1. State Management System

### State Machine Structure

```typescript
// Hierarchical States
├── busy (processing tasks)
└── ready (available for interaction)
    ├── planning (strategic thinking mode)
    ├── editing (code editing mode)
    └── generic (general conversation)
```

### Implementation Guide

#### 1.1 State Manager (`impl/state-manager.ts`)

**Purpose**: XState 5-based state manager with subscription system

**Key Features**:
- Hierarchical state management (busy/ready → sub-states)
- Shift+Tab cycling between ready sub-states
- Task lifecycle management
- Subscription-based state notifications

**Implementation Requirements**:
```typescript
// Required Dependencies
import { createActor } from 'xstate';
import { cliStateMachine } from '../abstractions/state-machine.js';

// Core Interface Implementation
class CLIStateManager implements IStateManager {
  private actor: CLIStateActor;
  private subscribers: Set<StateCallback> = new Set();
  
  // State access methods
  getCurrentState(): AppState
  getCurrentSubState(): AppSubState
  getStateContext(): AppStateContext
  
  // State transitions
  transition(event: StateEvent): void
  setBusy(taskName: string): void
  setReady(subState?: AppSubState): void
  cycleReadyStates(): void
  
  // Subscription management
  subscribe(callback: StateCallback): () => void
}
```

**Critical Implementation Notes**:
- Must use XState 5 syntax with `setup()` and `createMachine()`
- State cycling only allowed when `currentState === 'ready'`
- All state changes must notify subscribers immediately
- Session ID generation for debugging/tracking

#### 1.2 State Machine Definition (`abstractions/state-machine.ts`)

**Purpose**: XState 5 machine definition with hierarchical states

**Required Events**:
```typescript
type CLIStateEvent =
  | { type: 'START_TASK'; taskName: string }
  | { type: 'TASK_COMPLETE' }
  | { type: 'TASK_ERROR'; error?: string }
  | { type: 'CYCLE_STATES' }
  | { type: 'SET_STATE'; subState: AppSubState }
  | { type: 'RESET' };
```

**State Transitions**:
- `ready.generic` → `ready.planning` → `ready.editing` → `ready.generic` (cycling)
- Any ready sub-state → `busy` (on START_TASK)
- `busy` → previous sub-state (on TASK_COMPLETE/ERROR)

## 2. Command System

### 2.1 Command Parser & Handler (`impl/simple-command-handler.ts`)

**Purpose**: Handle CLI-specific commands only (help, exit, clear, version)

**Architecture**:
```typescript
// Command Structure
interface CLICommand {
  readonly name: string;
  readonly description: string; 
  readonly usage: string;
  readonly aliases?: readonly string[];
}

// Request/Response Pattern
interface CLICommandRequest {
  readonly command: string;
  readonly args: readonly string[];
  readonly rawInput: string;
}

interface CLICommandResult {
  readonly success: boolean;
  readonly output: string;
  readonly shouldExit?: boolean;
  readonly shouldClear?: boolean;
}
```

**Built-in Commands**:
- `/help` (aliases: `h`, `?`) - Show available commands
- `/exit` (aliases: `quit`, `q`) - Exit application
- `/clear` (aliases: `cls`) - Clear screen
- `/version` (aliases: `v`) - Show version info

**Implementation Requirements**:
```typescript
class SimpleCLICommandHandler {
  private commands: Map<string, CLICommand> = new Map();
  private aliases: Map<string, string> = new Map();
  
  // Core methods
  executeCommand(request: CLICommandRequest): CLICommandResult
  isCommand(input: string): boolean  // Check if starts with '/'
  parseCommand(input: string): CLICommandRequest | null
  getCommands(): readonly CLICommand[]
}
```

### 2.2 Command vs Agent Input Separation

**Critical Design Principle**: CLI handler only processes `/commands`, everything else goes to agent system via message queue.

```typescript
// Input Processing Flow
if (input.startsWith('/')) {
  // CLI command - handle locally
  const result = commandHandler.executeCommand(parseCommand(input));
  displayResult(result);
} else {
  // Agent input - send to message queue
  messageQueue.enqueue(createUserInputMessage(input));
}
```

## 3. Message-Driven Architecture

### 3.1 Integration with @qi/amsg

**Purpose**: All agent communication through async message queue

**Key Integrations**:
```typescript
import { QiAsyncMessageQueue } from '@qi/amsg/impl/QiAsyncMessageQueue';
import { MessageType, MessagePriority } from '@qi/amsg/types/MessageTypes';

// Message-Driven CLI Implementation
class MessageDrivenCLI implements ICLIFramework {
  constructor(
    private messageQueue: QiAsyncMessageQueue,
    private stateManager: IStateManager
  ) {}
  
  // Never call agent directly - always enqueue messages
  private async enqueueUserInput(input: string): Promise<void> {
    const message = messageFactory.createUserInputMessage(
      input, 
      'cli', 
      false
    );
    
    await this.messageQueue.enqueue(message);
  }
}
```

### 3.2 Message Types for CLI

**User Input Messages**:
```typescript
// User typed input (not CLI commands)
{
  type: 'USER_INPUT',
  input: string,
  source: 'cli',
  raw: false
}
```

**System Control Messages**:
```typescript
// State changes, mode cycling
{
  type: 'SYSTEM_CONTROL',
  action: 'pause' | 'resume' | 'reset',
  immediate: boolean
}
```

## 4. Keyboard Input Management

### 4.1 Hotkey Manager (`keyboard/HotkeyManager.ts`)

**Purpose**: Handle special keyboard inputs and send messages to queue

**Supported Hotkeys**:
- `Shift+Tab`: Cycle ready states (planning → editing → generic)
- `Esc`: Cancel current operation
- `Ctrl+C`: Graceful exit (SIGINT)

**Implementation Pattern**:
```typescript
class HotkeyManager {
  constructor(
    private messageQueue: QiAsyncMessageQueue,
    private config: HotkeyConfig
  ) {}
  
  // Never emit events - always enqueue messages
  private async handleShiftTab(): Promise<void> {
    const message = messageFactory.createSystemControlMessage(
      'cycle_states',
      false
    );
    await this.messageQueue.enqueue(message);
  }
}
```

### 4.2 Keyboard Utilities (`keyboard/KeyboardUtils.ts`)

**Purpose**: Low-level keyboard input detection and processing

**Key Functions**:
- `identifyKey(data: Buffer): KeyEvent` - Parse raw keyboard input
- `isControlCharacter(char: string): boolean` - Detect control chars
- `matchesEscapeSequence(data: Buffer): EscapeSequence | null` - Parse escape sequences

## 5. UI Framework Adapters

### 5.1 Framework Architecture

```typescript
// Framework Interface
interface ICLIFramework {
  initialize(config: CLIConfig): Promise<void>;
  start(): Promise<void>;
  stop(): Promise<void>;
  processInput(input: string): Promise<void>;
  getStatus(): CLIStatus;
}

// Available Frameworks
├── ReadlineFramework    # Node.js readline-based
├── InkFramework        # React-based terminal UI
└── HybridFramework     # Combines both approaches
```

### 5.2 Hybrid Framework (Recommended)

**Purpose**: Combines readline for input with custom rendering

**Architecture**:
- Readline interface for input handling
- Custom ANSI rendering for output
- Hotkey manager integration
- State-aware prompts

## 6. UI Components

### 6.1 Mode Indicator (`ui/ModeIndicator.ts`)

**Purpose**: Visual indication of current state/sub-state

**States**:
```typescript
const modeConfig = {
  ready: {
    planning: { emoji: '🧠', color: 'blue', label: 'Planning' },
    editing: { emoji: '✏️', color: 'green', label: 'Editing' },
    generic: { emoji: '💬', color: 'white', label: 'Generic' }
  },
  busy: { emoji: '⚙️', color: 'yellow', label: 'Processing' }
};
```

### 6.2 Progress Display (`ui/ProgressDisplay.ts`)

**Purpose**: Show task progress during busy state

**Features**:
- Spinner animations during indefinite tasks
- Progress bars for measurable tasks
- Time elapsed display
- Configurable throttling

### 6.3 Streaming Renderer (`ui/StreamingRenderer.ts`)

**Purpose**: Handle real-time output display

**Features**:
- Chunked text rendering
- Word wrapping
- Throttled updates
- ANSI escape sequence handling

## 7. Implementation Priorities

### Phase 1: Core Infrastructure
1. ✅ State machine and state manager
2. ✅ Command parser and handler
3. ✅ Message queue integration
4. ✅ Basic UI components

### Phase 2: Framework Implementation
1. **Readline Framework**: Basic terminal interface
2. **Hybrid Framework**: Enhanced user experience
3. **Hotkey Manager**: Keyboard shortcut handling

### Phase 3: Advanced Features
1. **Ink Framework**: React-based UI (optional)
2. **Theme System**: Customizable appearance
3. **Plugin Architecture**: Extensible command system

## 8. Integration with QiCore

### 8.1 Dependencies

```json
{
  "dependencies": {
    "@qi/base": "file:../base",      // Result<T> patterns
    "@qi/amsg": "file:../amsg",      // Async messaging
    "xstate": "^5.0.0",              // State management
    "readline": "built-in"           // Terminal interface
  }
}
```

### 8.2 Export Structure

```typescript
// Main exports from @qi/cli
export { MessageDrivenCLI } from './impl/MessageDrivenCLI';
export { createCLIAsync } from './factories/createCLI';
export { CLIStateManager } from './impl/state-manager';
export { SimpleCLICommandHandler } from './impl/simple-command-handler';

// UI Components
export { ModeIndicator } from './ui/ModeIndicator';
export { ProgressDisplay } from './ui/ProgressDisplay';
export { HotkeyManager } from './keyboard/HotkeyManager';

// Types
export type { CLIConfig, ICLIFramework, IStateManager } from './abstractions';
```

## 9. Testing Strategy

### 9.1 Unit Tests
- State machine transitions
- Command parsing and execution
- Message queue integration
- UI component rendering

### 9.2 Integration Tests
- Framework initialization
- User input processing
- State synchronization
- Hotkey handling

## 10. Implementation Notes

### Critical Success Factors

1. **Pure Message Architecture**: No direct agent calls, only message queue
2. **State Machine Correctness**: Proper XState 5 implementation
3. **Command Separation**: CLI commands vs agent inputs
4. **Framework Agnostic**: Support multiple UI frameworks
5. **QiCore Integration**: Proper Result<T> error handling

### Common Pitfalls to Avoid

1. **Direct Agent Calls**: Must use message queue for all agent communication
2. **Event Emission**: Use message queue instead of EventEmitter patterns
3. **Synchronous Operations**: All I/O should be async with Result<T> patterns
4. **State Mutations**: Use XState actions for all state changes
5. **Framework Lock-in**: Keep UI logic separate from business logic

This implementation guide provides the complete architecture and implementation details needed to build the @qi/cli package in the QiCore ecosystem.