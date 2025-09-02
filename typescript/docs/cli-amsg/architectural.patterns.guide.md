# Architectural Patterns Guide - CLI & Messaging

## Overview

This guide documents the key architectural patterns used in the @qi/cli and @qi/amsg packages, focusing on command parsing, state management, and message-driven communication patterns.

## 1. Command System Architecture

### 1.1 Command Separation Pattern

**Problem**: Need to distinguish between CLI interface commands and agent work commands.

**Solution**: Two-tier command system with clear separation of concerns.

```typescript
// Input Classification Pattern
class InputClassifier {
  classify(input: string): 'cli_command' | 'agent_input' {
    return input.trim().startsWith('/') ? 'cli_command' : 'agent_input';
  }
}

// Routing Pattern
class InputRouter {
  async route(input: string): Promise<void> {
    const classification = this.classifier.classify(input);
    
    if (classification === 'cli_command') {
      // Handle locally - no agent involvement
      await this.handleCLICommand(input);
    } else {
      // Send to agent via message queue
      await this.sendToAgent(input);
    }
  }
}
```

### 1.2 Command Parser Pattern

**Purpose**: Parse CLI commands with arguments and validation.

```typescript
// Command Request Pattern
interface CLICommandRequest {
  readonly command: string;
  readonly args: readonly string[];
  readonly rawInput: string;
  readonly flags?: Record<string, boolean>;
  readonly options?: Record<string, string>;
}

// Parser Implementation
class CommandParser {
  parseCommand(input: string): CLICommandRequest | null {
    if (!input.trim().startsWith('/')) return null;
    
    const tokens = this.tokenize(input.slice(1)); // Remove '/' prefix
    const command = tokens[0];
    const { args, flags, options } = this.parseTokens(tokens.slice(1));
    
    return {
      command,
      args,
      rawInput: input,
      flags,
      options
    };
  }
  
  private tokenize(input: string): string[] {
    // Handle quoted arguments: /help "complex argument"
    return input.match(/(?:[^\s"]+|"[^"]*")+/g) || [];
  }
  
  private parseTokens(tokens: string[]): {
    args: string[];
    flags: Record<string, boolean>;
    options: Record<string, string>;
  } {
    const args: string[] = [];
    const flags: Record<string, boolean> = {};
    const options: Record<string, string> = {};
    
    for (let i = 0; i < tokens.length; i++) {
      const token = tokens[i];
      
      if (token.startsWith('--')) {
        // Long option: --verbose or --output=file
        const [key, value] = token.slice(2).split('=', 2);
        if (value !== undefined) {
          options[key] = value;
        } else if (i + 1 < tokens.length && !tokens[i + 1].startsWith('-')) {
          options[key] = tokens[++i];
        } else {
          flags[key] = true;
        }
      } else if (token.startsWith('-')) {
        // Short flag: -v or -h
        flags[token.slice(1)] = true;
      } else {
        args.push(token.replace(/^"(.*)"$/, '$1')); // Remove quotes
      }
    }
    
    return { args, flags, options };
  }
}
```

### 1.3 Command Handler Pattern

**Purpose**: Execute CLI commands with validation and error handling.

```typescript
// Command Definition Pattern
interface CLICommand {
  readonly name: string;
  readonly description: string;
  readonly usage: string;
  readonly aliases?: readonly string[];
  readonly validator?: (request: CLICommandRequest) => Result<void, ValidationError>;
  readonly handler: (request: CLICommandRequest) => Promise<CLICommandResult>;
}

// Handler Registry Pattern
class CommandRegistry {
  private commands = new Map<string, CLICommand>();
  private aliases = new Map<string, string>();
  
  register(command: CLICommand): Result<void, RegistrationError> {
    // Validate command definition
    const validation = this.validateCommand(command);
    if (!validation.success) {
      return validation;
    }
    
    // Register command
    this.commands.set(command.name, command);
    
    // Register aliases
    if (command.aliases) {
      for (const alias of command.aliases) {
        if (this.aliases.has(alias)) {
          return failure(registrationError.aliasConflict(alias, command.name));
        }
        this.aliases.set(alias, command.name);
      }
    }
    
    return success(void 0);
  }
  
  execute(request: CLICommandRequest): Promise<Result<CLICommandResult, ExecutionError>> {
    const commandName = this.resolveAlias(request.command);
    const command = this.commands.get(commandName);
    
    if (!command) {
      return Promise.resolve(failure(
        executionError.commandNotFound(request.command)
      ));
    }
    
    return this.executeCommand(command, request);
  }
  
  private async executeCommand(
    command: CLICommand,
    request: CLICommandRequest
  ): Promise<Result<CLICommandResult, ExecutionError>> {
    // Validate arguments
    if (command.validator) {
      const validation = command.validator(request);
      if (!validation.success) {
        return failure(executionError.validationFailed(validation.error));
      }
    }
    
    try {
      const result = await command.handler(request);
      return success(result);
    } catch (error) {
      return failure(executionError.handlerFailed(error as Error));
    }
  }
}
```

## 2. State Management Patterns

### 2.1 Hierarchical State Machine Pattern

**Purpose**: Manage CLI application state with XState 5.

```typescript
// State Hierarchy
/*
CLI State Machine:
├── busy (processing)
└── ready (interactive)
    ├── planning (strategic mode)
    ├── editing (code mode) 
    └── generic (chat mode)
*/

// State Definition Pattern
export const cliStateMachine = setup({
  types: {} as {
    context: CLIStateContext;
    events: CLIStateEvent;
  },
  
  actions: {
    setBusyState: assign({
      currentState: 'busy' as AppState,
      startTime: () => new Date(),
    }),
    
    setReadyState: assign({
      currentState: 'ready' as AppState,
    }),
    
    cycleSubStates: assign(({ context }) => ({
      lastSubState: context.currentSubState,
      currentSubState: getNextSubState(context.currentSubState),
    })),
    
    saveCurrentSubState: assign({
      lastSubState: ({ context }) => context.currentSubState,
    }),
    
    restoreLastSubState: assign({
      currentSubState: ({ context }) => context.lastSubState,
    })
  },
  
  guards: {
    canCycleStates: ({ context }) => context.currentState === 'ready',
    isCriticalTask: ({ event }) => 
      event.type === 'START_TASK' && event.priority === 'critical'
  }
}).createMachine({
  id: 'cli',
  initial: 'ready',
  
  states: {
    busy: {
      entry: ['setBusyState'],
      on: {
        TASK_COMPLETE: { target: 'ready', actions: ['restoreLastSubState'] },
        TASK_ERROR: { target: 'ready', actions: ['restoreLastSubState'] },
        // Ignore state cycling when busy
        CYCLE_STATES: { actions: [] }
      }
    },
    
    ready: {
      entry: ['setReadyState'],
      initial: 'generic',
      
      states: {
        planning: {
          on: { CYCLE_STATES: { target: 'editing', actions: ['saveCurrentSubState'] } }
        },
        editing: {
          on: { CYCLE_STATES: { target: 'generic', actions: ['saveCurrentSubState'] } }
        },
        generic: {
          on: { CYCLE_STATES: { target: 'planning', actions: ['saveCurrentSubState'] } }
        }
      },
      
      on: {
        START_TASK: { target: 'busy', actions: ['saveCurrentSubState'] }
      }
    }
  }
});
```

### 2.2 State Manager Facade Pattern

**Purpose**: Provide simple interface over complex state machine.

```typescript
// Facade Interface
interface IStateManager {
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

// Implementation Pattern
class CLIStateManager implements IStateManager {
  private actor: CLIStateActor;
  private subscribers = new Set<StateCallback>();
  
  constructor(sessionId?: string) {
    this.actor = createActor(cliStateMachine, {
      input: createInitialContext(sessionId || this.generateSessionId())
    });
    
    // Subscribe to state changes
    this.actor.subscribe((snapshot) => {
      this.notifySubscribers(this.mapSnapshot(snapshot));
    });
    
    this.actor.start();
  }
  
  setBusy(taskName: string): void {
    this.actor.send({ type: 'START_TASK', taskName });
  }
  
  setReady(subState?: AppSubState): void {
    if (subState) {
      this.actor.send({ type: 'SET_STATE', subState });
    }
    this.actor.send({ type: 'TASK_COMPLETE' });
  }
  
  cycleReadyStates(): void {
    if (this.canCycleStates()) {
      this.actor.send({ type: 'CYCLE_STATES' });
    }
  }
  
  subscribe(callback: StateCallback): () => void {
    this.subscribers.add(callback);
    
    // Immediate callback with current state
    callback(this.getStateContext());
    
    return () => this.subscribers.delete(callback);
  }
  
  private canCycleStates(): boolean {
    return this.actor.getSnapshot().context.currentState === 'ready';
  }
  
  private notifySubscribers(context: AppStateContext): void {
    for (const callback of this.subscribers) {
      try {
        callback(context);
      } catch (error) {
        console.error('State subscriber error:', error);
      }
    }
  }
}
```

## 3. Message-Driven Architecture Patterns

### 3.1 Publisher-Subscriber via Message Queue

**Purpose**: Decouple CLI from agent processing via async messaging.

```typescript
// Publisher Pattern (CLI side)
class CLIMessagePublisher {
  constructor(private messageQueue: QiAsyncMessageQueue) {}
  
  async publishUserInput(input: string): Promise<Result<void, PublishError>> {
    const messageResult = messageFactory.createUserInputMessage(input, 'cli');
    
    return flatMap(
      async (message) => this.messageQueue.enqueue(message),
      messageResult
    );
  }
  
  async publishSystemControl(
    action: SystemAction, 
    immediate = false
  ): Promise<Result<void, PublishError>> {
    const messageResult = messageFactory.createSystemControlMessage(
      action, 
      immediate
    );
    
    return flatMap(
      async (message) => this.messageQueue.enqueue(message),
      messageResult
    );
  }
}

// Subscriber Pattern (Agent side)
class AgentMessageSubscriber {
  constructor(private messageQueue: QiAsyncMessageQueue) {}
  
  async startProcessing(): Promise<void> {
    try {
      // h2A pattern - async iteration
      for await (const message of this.messageQueue) {
        await this.handleMessage(message);
      }
    } catch (error) {
      console.error('Message processing error:', error);
    }
  }
  
  private async handleMessage(message: QiMessage): Promise<void> {
    const handler = this.getHandler(message.type);
    
    if (handler) {
      const result = await handler(message);
      
      match(
        () => console.log(`Processed ${message.type} successfully`),
        (error) => this.handleError(error, message),
        result
      );
    }
  }
}
```

### 3.2 Request-Response Correlation Pattern

**Purpose**: Track request/response pairs in async messaging system.

```typescript
// Correlation Manager
class MessageCorrelationManager {
  private pendingRequests = new Map<string, PendingRequest>();
  
  async sendRequest<TResponse>(
    request: QiMessage,
    timeout = 30000
  ): Promise<Result<TResponse, CorrelationError>> {
    const correlationId = request.correlationId || request.id;
    
    // Set up response waiting
    const responsePromise = this.waitForResponse<TResponse>(correlationId, timeout);
    
    // Send request
    const sendResult = await this.messageQueue.enqueue(request);
    
    if (!sendResult.success) {
      return failure(correlationError.sendFailed(sendResult.error));
    }
    
    // Wait for response
    return responsePromise;
  }
  
  private waitForResponse<TResponse>(
    correlationId: string,
    timeout: number
  ): Promise<Result<TResponse, CorrelationError>> {
    return new Promise((resolve) => {
      const timer = setTimeout(() => {
        this.pendingRequests.delete(correlationId);
        resolve(failure(correlationError.timeout(correlationId)));
      }, timeout);
      
      this.pendingRequests.set(correlationId, {
        resolve,
        timer,
        startTime: Date.now()
      });
    });
  }
  
  handleResponse(response: QiMessage): void {
    const correlationId = response.correlationId;
    
    if (!correlationId) {
      console.warn('Received response without correlation ID');
      return;
    }
    
    const pending = this.pendingRequests.get(correlationId);
    
    if (pending) {
      clearTimeout(pending.timer);
      this.pendingRequests.delete(correlationId);
      pending.resolve(success(response));
    }
  }
}
```

### 3.3 Message Priority Processing Pattern

**Purpose**: Handle critical messages before normal operations.

```typescript
// Priority Queue Implementation
class PriorityMessageQueue {
  private queues = new Map<MessagePriority, QiMessage[]>();
  
  constructor() {
    // Initialize priority queues
    for (const priority of Object.values(MessagePriority)) {
      this.queues.set(priority, []);
    }
  }
  
  enqueue(message: QiMessage): Result<void, QueueError> {
    const priorityQueue = this.queues.get(message.priority);
    
    if (!priorityQueue) {
      return failure(queueError.invalidPriority(message.priority));
    }
    
    priorityQueue.push(message);
    return success(void 0);
  }
  
  dequeue(): Result<QiMessage | null, QueueError> {
    // Process in priority order: CRITICAL → HIGH → NORMAL → LOW
    for (const priority of [
      MessagePriority.CRITICAL,
      MessagePriority.HIGH,
      MessagePriority.NORMAL,
      MessagePriority.LOW
    ]) {
      const queue = this.queues.get(priority);
      
      if (queue && queue.length > 0) {
        const message = queue.shift()!;
        return success(message);
      }
    }
    
    return success(null); // No messages
  }
  
  // Emergency priority for critical system messages
  enqueueCritical(message: QiMessage): Result<void, QueueError> {
    // Critical messages can interrupt processing
    const criticalQueue = this.queues.get(MessagePriority.CRITICAL);
    
    if (criticalQueue) {
      // Add to front of critical queue
      criticalQueue.unshift({ ...message, priority: MessagePriority.CRITICAL });
      return success(void 0);
    }
    
    return failure(queueError.criticalQueueUnavailable());
  }
}
```

## 4. Error Handling Patterns

### 4.1 Result<T> Error Propagation Pattern

**Purpose**: Functional error handling throughout the system.

```typescript
// Chain of operations with error handling
async function processUserCommand(input: string): Promise<Result<string, ProcessingError>> {
  // Parse command
  const parseResult = commandParser.parseCommand(input);
  if (!parseResult) {
    return failure(processingError.invalidCommand(input));
  }
  
  // Validate command
  const validationResult = await validateCommand(parseResult);
  
  return flatMap(
    async (validatedCommand) => {
      // Execute command
      const executionResult = await executeCommand(validatedCommand);
      
      return match(
        (result) => success(`Command executed: ${result.output}`),
        (error) => failure(processingError.executionFailed(error)),
        executionResult
      );
    },
    validationResult
  );
}

// Usage with proper error handling
const result = await processUserCommand('/help');

match(
  (output) => console.log(output),
  (error) => {
    console.error('Command processing failed:', error.message);
    // Log full error context for debugging
    console.debug('Error context:', error.context);
  },
  result
);
```

### 4.2 Error Recovery Pattern

**Purpose**: Graceful degradation and recovery from errors.

```typescript
// Retry with exponential backoff
async function withRetry<T>(
  operation: () => Promise<Result<T, RetriableError>>,
  maxAttempts = 3,
  baseDelay = 1000
): Promise<Result<T, RetriableError>> {
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    const result = await operation();
    
    if (result.success) {
      return result;
    }
    
    // Check if error is retriable
    if (!result.error.retriable || attempt === maxAttempts) {
      return result;
    }
    
    // Wait before retry with exponential backoff
    const delay = baseDelay * Math.pow(2, attempt - 1);
    await new Promise(resolve => setTimeout(resolve, delay));
    
    console.warn(`Retry attempt ${attempt}/${maxAttempts} after ${delay}ms`);
  }
  
  // This should never be reached, but TypeScript needs it
  return failure(retriableError.maxAttemptsExceeded(maxAttempts));
}

// Circuit breaker pattern for external dependencies
class CircuitBreaker<T> {
  private failures = 0;
  private lastFailureTime = 0;
  private state: 'CLOSED' | 'OPEN' | 'HALF_OPEN' = 'CLOSED';
  
  constructor(
    private operation: () => Promise<Result<T, any>>,
    private failureThreshold = 5,
    private recoveryTimeout = 30000
  ) {}
  
  async execute(): Promise<Result<T, CircuitBreakerError>> {
    if (this.state === 'OPEN') {
      if (Date.now() - this.lastFailureTime > this.recoveryTimeout) {
        this.state = 'HALF_OPEN';
      } else {
        return failure(circuitBreakerError.circuitOpen());
      }
    }
    
    const result = await this.operation();
    
    if (result.success) {
      this.onSuccess();
      return result;
    } else {
      this.onFailure();
      return failure(circuitBreakerError.operationFailed(result.error));
    }
  }
  
  private onSuccess(): void {
    this.failures = 0;
    this.state = 'CLOSED';
  }
  
  private onFailure(): void {
    this.failures++;
    this.lastFailureTime = Date.now();
    
    if (this.failures >= this.failureThreshold) {
      this.state = 'OPEN';
    }
  }
}
```

## 5. Integration Patterns

### 5.1 Dependency Injection Pattern

**Purpose**: Maintain loose coupling between components.

```typescript
// Interface-based dependency injection
interface CLIDependencies {
  messageQueue: QiAsyncMessageQueue;
  stateManager: IStateManager;
  commandHandler: ICommandHandler;
  keyboardManager: IKeyboardManager;
  uiRenderer: IUIRenderer;
}

class MessageDrivenCLI {
  constructor(private dependencies: CLIDependencies) {}
  
  async initialize(): Promise<Result<void, InitializationError>> {
    // Initialize all components
    const results = await Promise.all([
      this.dependencies.messageQueue.start(),
      this.dependencies.stateManager.initialize(),
      this.dependencies.uiRenderer.initialize()
    ]);
    
    // Check for any initialization failures
    const failures = results.filter(r => !r.success);
    
    if (failures.length > 0) {
      return failure(initializationError.componentsFailed(failures));
    }
    
    return success(void 0);
  }
}

// Factory pattern for dependency creation
class CLIDependencyFactory {
  static create(config: CLIConfig): CLIDependencies {
    const messageQueue = new QiAsyncMessageQueue(config.queueOptions);
    const stateManager = new CLIStateManager(config.sessionId);
    const commandHandler = new SimpleCLICommandHandler();
    const keyboardManager = new HotkeyManager(messageQueue, config.hotkeyConfig);
    const uiRenderer = this.createRenderer(config.framework);
    
    return {
      messageQueue,
      stateManager,
      commandHandler,
      keyboardManager,
      uiRenderer
    };
  }
  
  private static createRenderer(framework: CLIFramework): IUIRenderer {
    switch (framework) {
      case 'readline':
        return new ReadlineRenderer();
      case 'ink':
        return new InkRenderer();
      case 'hybrid':
        return new HybridRenderer();
      default:
        return new ReadlineRenderer(); // Default fallback
    }
  }
}
```

This architectural patterns guide provides the foundational patterns needed to implement robust CLI and messaging systems with proper separation of concerns, error handling, and scalability.