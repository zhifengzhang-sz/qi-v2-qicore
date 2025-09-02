# Integration Examples & Usage Patterns

## Overview

This guide provides complete working examples of how to integrate @qi/cli and @qi/amsg packages with practical usage patterns, initialization sequences, and real-world scenarios.

## 1. Basic CLI Application Setup

### 1.1 Complete Minimal Example

```typescript
// main.ts - Complete CLI application setup
import { QiAsyncMessageQueue } from '@qi/amsg';
import { 
  MessageDrivenCLI, 
  CLIStateManager, 
  SimpleCLICommandHandler,
  createCLIAsync 
} from '@qi/cli';
import { success, failure, match } from '@qi/base';

async function createMinimalCLI(): Promise<void> {
  // 1. Create message queue with basic configuration
  const messageQueue = new QiAsyncMessageQueue({
    maxSize: 1000,
    maxConcurrentProcessing: 5,
    processingTimeout: 30000
  });

  // 2. Create state manager
  const stateManager = new CLIStateManager('session-' + Date.now());

  // 3. Create command handler
  const commandHandler = new SimpleCLICommandHandler();

  // 4. Create CLI instance
  const cli = new MessageDrivenCLI({
    messageQueue,
    stateManager,
    commandHandler,
    config: {
      framework: 'hybrid',
      enableHotkeys: true,
      enableModeIndicator: true
    }
  });

  // 5. Set up message processing
  const messageProcessor = new AgentMessageProcessor(messageQueue);
  
  // 6. Start systems
  const startResult = await startSystems({
    messageQueue,
    cli,
    messageProcessor
  });

  match(
    () => console.log('✅ CLI started successfully'),
    (error) => {
      console.error('❌ Failed to start CLI:', error);
      process.exit(1);
    },
    startResult
  );
}

async function startSystems(systems: {
  messageQueue: QiAsyncMessageQueue;
  cli: MessageDrivenCLI;
  messageProcessor: AgentMessageProcessor;
}): Promise<Result<void, SystemError>> {
  try {
    // Start message queue
    const queueResult = await systems.messageQueue.start();
    if (!queueResult.success) {
      return failure(systemError.queueStartFailed(queueResult.error));
    }

    // Start message processor in background
    systems.messageProcessor.startProcessing().catch(error => {
      console.error('Message processor error:', error);
    });

    // Start CLI
    const cliResult = await systems.cli.start();
    if (!cliResult.success) {
      return failure(systemError.cliStartFailed(cliResult.error));
    }

    return success(void 0);
  } catch (error) {
    return failure(systemError.unexpectedError(error as Error));
  }
}

// Bootstrap the application
createMinimalCLI().catch(console.error);
```

### 1.2 Message Processor Implementation

```typescript
// agent-message-processor.ts
class AgentMessageProcessor {
  constructor(
    private messageQueue: QiAsyncMessageQueue,
    private agentService?: IAgentService
  ) {}

  async startProcessing(): Promise<void> {
    console.log('🔄 Starting message processor...');
    
    try {
      // h2A pattern - process messages as they arrive
      for await (const message of this.messageQueue) {
        await this.handleMessage(message);
      }
    } catch (error) {
      console.error('💥 Message processing error:', error);
      throw error;
    }
  }

  private async handleMessage(message: QiMessage): Promise<void> {
    const startTime = Date.now();
    
    try {
      switch (message.type) {
        case MessageType.USER_INPUT:
          await this.handleUserInput(message as UserInputMessage);
          break;
        
        case MessageType.COMMAND:
          await this.handleCommand(message as CommandMessage);
          break;
        
        case MessageType.SYSTEM_CONTROL:
          await this.handleSystemControl(message as SystemControlMessage);
          break;
        
        default:
          console.warn(`⚠️ Unhandled message type: ${message.type}`);
      }
    } catch (error) {
      console.error(`❌ Error processing ${message.type}:`, error);
      
      // Send error response if needed
      await this.sendErrorResponse(message, error as Error);
    } finally {
      const duration = Date.now() - startTime;
      console.debug(`⏱️ Processed ${message.type} in ${duration}ms`);
    }
  }

  private async handleUserInput(message: UserInputMessage): Promise<void> {
    if (!this.agentService) {
      console.log(`💬 User: ${message.input}`);
      console.log(`🤖 Agent: (No agent service configured)`);
      return;
    }

    // Send to agent service
    const response = await this.agentService.processInput(message.input);
    
    match(
      async (content) => {
        // Send response back through message queue
        const responseMessage = messageFactory.createAgentOutputMessage(
          content,
          'text',
          false
        );
        
        if (responseMessage.success) {
          await this.messageQueue.enqueue(responseMessage.value);
        }
      },
      (error) => {
        console.error('Agent processing error:', error);
      },
      response
    );
  }

  private async handleSystemControl(message: SystemControlMessage): Promise<void> {
    switch (message.action) {
      case 'pause':
        await this.messageQueue.pause();
        break;
      case 'resume':
        await this.messageQueue.resume();
        break;
      case 'reset':
        console.log('🔄 System reset requested');
        break;
      case 'shutdown':
        console.log('🛑 Shutdown requested');
        process.exit(0);
    }
  }

  private async sendErrorResponse(originalMessage: QiMessage, error: Error): Promise<void> {
    if (originalMessage.type === MessageType.COMMAND) {
      const errorMessage = messageFactory.createErrorMessage(
        originalMessage as CommandMessage,
        create('PROCESSING_ERROR', error.message, 'AGENT', {
          originalMessageId: originalMessage.id,
          timestamp: new Date().toISOString()
        })
      );

      if (errorMessage.success) {
        await this.messageQueue.enqueue(errorMessage.value);
      }
    }
  }
}
```

## 2. Advanced CLI Configuration

### 2.1 Production Configuration

```typescript
// cli-config.ts
export interface ProductionCLIConfig {
  queue: {
    maxSize: number;
    maxConcurrentProcessing: number;
    processingTimeout: number;
    retryAttempts: number;
    retryDelay: number;
  };
  
  cli: {
    framework: 'hybrid' | 'readline' | 'ink';
    theme: CLITheme;
    enableHotkeys: boolean;
    enableModeIndicator: boolean;
    enableProgressDisplay: boolean;
    historySize: number;
  };
  
  logging: {
    level: 'debug' | 'info' | 'warn' | 'error';
    enableFileLogging: boolean;
    logDirectory?: string;
  };
  
  features: {
    enableAutoComplete: boolean;
    enableShellCommands: boolean;
    enableSessionPersistence: boolean;
  };
}

// production-setup.ts
async function createProductionCLI(config: ProductionCLIConfig): Promise<MessageDrivenCLI> {
  // 1. Enhanced message queue with monitoring
  const messageQueue = new QiAsyncMessageQueue({
    ...config.queue,
    enableMetrics: true,
    enableLogging: true
  });

  // 2. State manager with persistence
  const stateManager = new CLIStateManager('prod-' + generateSessionId());
  
  // 3. Enhanced command handler with custom commands
  const commandHandler = new EnhancedCLICommandHandler({
    enableShellCommands: config.features.enableShellCommands,
    customCommands: [
      {
        name: 'status',
        description: 'Show system status',
        usage: '/status [--detailed]',
        handler: async (request) => {
          const stats = messageQueue.getStats();
          const state = stateManager.getStateContext();
          
          return {
            success: true,
            output: formatSystemStatus(stats, state, request.flags?.detailed)
          };
        }
      },
      
      {
        name: 'metrics',
        description: 'Show performance metrics',
        usage: '/metrics [--reset]',
        handler: async (request) => {
          const metrics = await collectSystemMetrics();
          
          if (request.flags?.reset) {
            await resetMetrics();
          }
          
          return {
            success: true,
            output: formatMetrics(metrics)
          };
        }
      }
    ]
  });

  // 4. Advanced UI components
  const modeIndicator = new ModeIndicator({
    theme: config.cli.theme,
    enableAnimations: true,
    updateInterval: 100
  });

  const progressDisplay = new ProgressDisplay({
    enableSpinners: true,
    enableProgressBars: true,
    theme: config.cli.theme
  });

  // 5. Hotkey manager with custom shortcuts
  const hotkeyManager = new HotkeyManager(messageQueue, {
    enableShiftTab: config.cli.enableHotkeys,
    enableEscape: config.cli.enableHotkeys,
    enableCtrlC: true,
    customHotkeys: [
      {
        key: 'F1',
        action: () => commandHandler.executeCommand({ 
          command: 'help', 
          args: [], 
          rawInput: '/help' 
        })
      },
      {
        key: 'F2',
        action: () => stateManager.cycleReadyStates()
      }
    ]
  });

  // 6. Create CLI with all components
  const cli = new MessageDrivenCLI({
    messageQueue,
    stateManager,
    commandHandler,
    modeIndicator,
    progressDisplay,
    hotkeyManager,
    config: config.cli
  });

  return cli;
}
```

### 2.2 Custom Framework Implementation

```typescript
// custom-framework.ts - Example of custom UI framework
class CustomReactCLI implements ICLIFramework {
  private app?: App;
  private isRunning = false;
  
  constructor(
    private messageQueue: QiAsyncMessageQueue,
    private stateManager: IStateManager
  ) {}

  async initialize(config: CLIConfig): Promise<Result<void, InitializationError>> {
    try {
      // Set up React Ink application
      this.app = render(
        <CLIApp 
          messageQueue={this.messageQueue}
          stateManager={this.stateManager}
          config={config}
        />
      );

      return success(void 0);
    } catch (error) {
      return failure(initializationError.frameworkFailed(error as Error));
    }
  }

  async start(): Promise<Result<void, StartupError>> {
    this.isRunning = true;
    console.log('🚀 Custom React CLI started');
    return success(void 0);
  }

  async stop(): Promise<Result<void, ShutdownError>> {
    if (this.app) {
      this.app.unmount();
    }
    this.isRunning = false;
    return success(void 0);
  }

  getStatus(): CLIStatus {
    return {
      isRunning: this.isRunning,
      currentState: this.stateManager.getCurrentState(),
      currentSubState: this.stateManager.getCurrentSubState(),
      uptime: Date.now() - this.startTime,
      commandsExecuted: this.commandCount,
      errors: this.errorCount
    };
  }
}

// React component for Ink-based CLI
const CLIApp: React.FC<{
  messageQueue: QiAsyncMessageQueue;
  stateManager: IStateManager;
  config: CLIConfig;
}> = ({ messageQueue, stateManager, config }) => {
  const [state, setState] = useState(stateManager.getStateContext());
  const [messages, setMessages] = useState<string[]>([]);

  useEffect(() => {
    // Subscribe to state changes
    const unsubscribe = stateManager.subscribe(setState);
    return unsubscribe;
  }, [stateManager]);

  useEffect(() => {
    // Listen for agent output messages
    const processMessages = async () => {
      for await (const message of messageQueue) {
        if (message.type === MessageType.AGENT_OUTPUT) {
          const agentMsg = message as AgentOutputMessage;
          setMessages(prev => [...prev, `🤖 ${agentMsg.content}`]);
        }
      }
    };
    
    processMessages().catch(console.error);
  }, [messageQueue]);

  return (
    <Box flexDirection="column" height="100%">
      {/* Header */}
      <Box borderStyle="single" paddingX={1}>
        <Text color="cyan">Qi Agent CLI</Text>
        <Box marginLeft="auto">
          <Text color={getStateColor(state)}>
            {getStateEmoji(state)} {getStateLabel(state)}
          </Text>
        </Box>
      </Box>

      {/* Message Area */}
      <Box flexGrow={1} flexDirection="column" paddingX={1}>
        {messages.map((msg, i) => (
          <Text key={i}>{msg}</Text>
        ))}
      </Box>

      {/* Input Area */}
      <Box borderStyle="single" paddingX={1}>
        <TextInput
          placeholder={getPromptText(state)}
          onSubmit={handleUserInput}
        />
      </Box>
    </Box>
  );

  async function handleUserInput(input: string): Promise<void> {
    // Add user message to display
    setMessages(prev => [...prev, `👤 ${input}`]);

    // Send to message queue
    const message = messageFactory.createUserInputMessage(input, 'cli');
    
    if (message.success) {
      await messageQueue.enqueue(message.value);
    }
  }
};
```

## 3. Testing Patterns

### 3.1 Unit Testing Example

```typescript
// cli.test.ts
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { QiAsyncMessageQueue } from '@qi/amsg';
import { CLIStateManager, SimpleCLICommandHandler } from '@qi/cli';

describe('CLI Integration Tests', () => {
  let messageQueue: QiAsyncMessageQueue;
  let stateManager: CLIStateManager;
  let commandHandler: SimpleCLICommandHandler;

  beforeEach(async () => {
    messageQueue = new QiAsyncMessageQueue({
      maxSize: 100,
      maxConcurrentProcessing: 1
    });
    
    stateManager = new CLIStateManager('test-session');
    commandHandler = new SimpleCLICommandHandler();
    
    await messageQueue.start();
  });

  afterEach(async () => {
    await messageQueue.stop();
    stateManager.stop();
  });

  it('should process user input through message queue', async () => {
    // Arrange
    const testInput = 'Hello, agent!';
    let receivedMessage: QiMessage | null = null;

    // Set up message listener
    const messagePromise = new Promise<QiMessage>((resolve) => {
      const processMessage = async () => {
        for await (const message of messageQueue) {
          resolve(message);
          break;
        }
      };
      processMessage();
    });

    // Act
    const userMessage = messageFactory.createUserInputMessage(testInput, 'cli');
    expect(userMessage.success).toBe(true);
    
    await messageQueue.enqueue(userMessage.value!);
    receivedMessage = await messagePromise;

    // Assert
    expect(receivedMessage).toBeTruthy();
    expect(receivedMessage!.type).toBe(MessageType.USER_INPUT);
    expect((receivedMessage as UserInputMessage).input).toBe(testInput);
  });

  it('should handle CLI commands locally', async () => {
    // Arrange
    const helpCommand = '/help';

    // Act
    const isCommand = commandHandler.isCommand(helpCommand);
    const parseResult = commandHandler.parseCommand(helpCommand);
    const executeResult = commandHandler.executeCommand(parseResult!);

    // Assert
    expect(isCommand).toBe(true);
    expect(parseResult).toBeTruthy();
    expect(parseResult!.command).toBe('help');
    expect(executeResult.success).toBe(true);
    expect(executeResult.output).toContain('Available CLI Commands');
  });

  it('should manage state transitions correctly', async () => {
    // Arrange
    const initialState = stateManager.getCurrentState();
    const initialSubState = stateManager.getCurrentSubState();

    // Act - Start a task
    stateManager.setBusy('test task');
    const busyState = stateManager.getCurrentState();

    // Complete the task
    stateManager.setReady();
    const readyState = stateManager.getCurrentState();
    const finalSubState = stateManager.getCurrentSubState();

    // Assert
    expect(initialState).toBe('ready');
    expect(initialSubState).toBe('generic');
    expect(busyState).toBe('busy');
    expect(readyState).toBe('ready');
    expect(finalSubState).toBe('generic'); // Should restore previous sub-state
  });

  it('should cycle sub-states when ready', async () => {
    // Arrange
    expect(stateManager.getCurrentState()).toBe('ready');
    expect(stateManager.getCurrentSubState()).toBe('generic');

    // Act & Assert - Cycle through states
    stateManager.cycleReadyStates();
    expect(stateManager.getCurrentSubState()).toBe('planning');

    stateManager.cycleReadyStates();
    expect(stateManager.getCurrentSubState()).toBe('editing');

    stateManager.cycleReadyStates();
    expect(stateManager.getCurrentSubState()).toBe('generic');
  });

  it('should not cycle states when busy', async () => {
    // Arrange
    stateManager.setBusy('test task');
    expect(stateManager.getCurrentState()).toBe('busy');

    // Act
    stateManager.cycleReadyStates();

    // Assert - State should not change
    expect(stateManager.getCurrentState()).toBe('busy');
  });
});
```

### 3.2 Integration Testing Example

```typescript
// integration.test.ts
describe('End-to-End CLI Integration', () => {
  let testCLI: MessageDrivenCLI;
  let mockAgentService: MockAgentService;

  beforeEach(async () => {
    mockAgentService = new MockAgentService();
    testCLI = await createTestCLI(mockAgentService);
    await testCLI.start();
  });

  afterEach(async () => {
    await testCLI.stop();
  });

  it('should handle complete user interaction flow', async () => {
    // Test scenario: User asks question, gets response, asks follow-up
    const conversation = [
      { input: 'What is TypeScript?', expectedResponse: 'TypeScript is...' },
      { input: 'Can you give an example?', expectedResponse: 'Here is an example...' }
    ];

    for (const turn of conversation) {
      // Simulate user input
      const response = await simulateUserInput(testCLI, turn.input);
      
      // Wait for agent response
      const agentResponse = await waitForAgentResponse(testCLI);
      
      expect(agentResponse).toContain(turn.expectedResponse);
    }
  });

  it('should handle system control messages', async () => {
    // Test pause/resume functionality
    await testCLI.sendSystemControl('pause');
    
    // Queue should be paused
    const status = testCLI.getStatus();
    expect(status.queueState).toBe('paused');

    await testCLI.sendSystemControl('resume');
    
    // Queue should be running again
    const resumedStatus = testCLI.getStatus();
    expect(resumedStatus.queueState).toBe('running');
  });

  async function simulateUserInput(cli: MessageDrivenCLI, input: string): Promise<void> {
    return cli.processInput(input);
  }

  async function waitForAgentResponse(cli: MessageDrivenCLI, timeout = 5000): Promise<string> {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error('Response timeout')), timeout);
      
      // Listen for agent output messages
      cli.subscribe((message) => {
        if (message.type === MessageType.AGENT_OUTPUT) {
          clearTimeout(timer);
          resolve((message as AgentOutputMessage).content);
        }
      });
    });
  }
});

class MockAgentService implements IAgentService {
  async processInput(input: string): Promise<Result<string, AgentError>> {
    // Simulate processing delay
    await new Promise(resolve => setTimeout(resolve, 100));
    
    // Mock responses based on input
    const responses: Record<string, string> = {
      'What is TypeScript?': 'TypeScript is a strongly typed programming language that builds on JavaScript.',
      'Can you give an example?': 'Here is an example:\n```typescript\ninterface User {\n  name: string;\n  age: number;\n}\n```',
      'default': 'I understand your question. Let me help you with that.'
    };

    const response = responses[input] || responses.default;
    return success(response);
  }
}
```

## 4. Performance Optimization Patterns

### 4.1 Message Queue Optimization

```typescript
// optimized-queue.ts
class OptimizedMessageQueue extends QiAsyncMessageQueue {
  private processingPool: WorkerPool<QiMessage>;
  private messageCache: LRUCache<string, QiMessage>;
  private metrics: QueueMetrics;

  constructor(options: OptimizedQueueOptions) {
    super(options);
    
    this.processingPool = new WorkerPool({
      size: options.workerPoolSize || 4,
      taskHandler: this.processMessageInWorker.bind(this)
    });
    
    this.messageCache = new LRUCache<string, QiMessage>({
      max: options.cacheSize || 1000,
      ttl: options.cacheTTL || 300000 // 5 minutes
    });
    
    this.metrics = new QueueMetrics();
  }

  async enqueue(message: QiMessage): Promise<Result<void, QueueError>> {
    const startTime = performance.now();
    
    // Check for duplicate messages
    if (this.messageCache.has(message.id)) {
      return failure(queueError.duplicateMessage(message.id));
    }
    
    // Add to cache
    this.messageCache.set(message.id, message);
    
    // Enqueue with performance tracking
    const result = await super.enqueue(message);
    
    this.metrics.recordEnqueue(performance.now() - startTime, result.success);
    return result;
  }

  protected async processMessage(message: QiMessage): Promise<void> {
    // Use worker pool for CPU-intensive processing
    if (this.isCPUIntensive(message)) {
      await this.processingPool.execute(message);
    } else {
      await super.processMessage(message);
    }
  }

  private isCPUIntensive(message: QiMessage): boolean {
    // Determine if message requires heavy processing
    return message.type === MessageType.COMMAND && 
           (message as CommandMessage).command.startsWith('analyze');
  }

  private async processMessageInWorker(message: QiMessage): Promise<void> {
    // Worker processing logic
    switch (message.type) {
      case MessageType.COMMAND:
        return this.processCommandInWorker(message as CommandMessage);
      default:
        return this.processMessageDirectly(message);
    }
  }

  getPerformanceMetrics(): QueuePerformanceMetrics {
    return {
      ...this.metrics.getMetrics(),
      cacheHitRatio: this.messageCache.calculatedHitRatio,
      workerPoolUtilization: this.processingPool.getUtilization()
    };
  }
}
```

### 4.2 Memory Management

```typescript
// memory-management.ts
class MemoryManagedCLI extends MessageDrivenCLI {
  private memoryMonitor: MemoryMonitor;
  private resourceManager: ResourceManager;

  constructor(dependencies: CLIDependencies) {
    super(dependencies);
    
    this.memoryMonitor = new MemoryMonitor({
      warningThreshold: 0.8,  // 80% of max heap
      criticalThreshold: 0.9, // 90% of max heap
      checkInterval: 10000    // Check every 10 seconds
    });
    
    this.resourceManager = new ResourceManager({
      maxMessageHistory: 10000,
      maxCacheSize: 5000,
      gcInterval: 30000 // Force GC every 30 seconds
    });
  }

  async initialize(): Promise<Result<void, InitializationError>> {
    const result = await super.initialize();
    
    if (result.success) {
      await this.startMemoryManagement();
    }
    
    return result;
  }

  private async startMemoryManagement(): Promise<void> {
    // Monitor memory usage
    this.memoryMonitor.on('warning', (usage) => {
      console.warn(`⚠️ Memory usage: ${(usage.percentage * 100).toFixed(1)}%`);
      this.optimizeMemoryUsage();
    });

    this.memoryMonitor.on('critical', (usage) => {
      console.error(`🚨 Critical memory usage: ${(usage.percentage * 100).toFixed(1)}%`);
      this.emergencyMemoryCleanup();
    });

    this.memoryMonitor.start();
    
    // Periodic resource cleanup
    setInterval(() => {
      this.resourceManager.cleanup();
    }, this.resourceManager.options.gcInterval);
  }

  private optimizeMemoryUsage(): void {
    // Trim message history
    this.resourceManager.trimMessageHistory();
    
    // Clear expired cache entries
    this.resourceManager.clearExpiredCache();
    
    // Suggest garbage collection
    if (global.gc) {
      global.gc();
    }
  }

  private emergencyMemoryCleanup(): void {
    // More aggressive cleanup
    this.resourceManager.emergencyCleanup();
    
    // Force garbage collection
    if (global.gc) {
      global.gc();
    }
    
    // Pause non-critical operations
    this.messageQueue.pause();
    
    setTimeout(() => {
      this.messageQueue.resume();
    }, 5000);
  }
}

class ResourceManager {
  private messageHistory: QiMessage[] = [];
  private cache = new Map<string, any>();

  constructor(public options: ResourceManagerOptions) {}

  addMessage(message: QiMessage): void {
    this.messageHistory.push(message);
    
    // Trim if exceeding limit
    if (this.messageHistory.length > this.options.maxMessageHistory) {
      this.messageHistory.splice(0, this.messageHistory.length - this.options.maxMessageHistory);
    }
  }

  trimMessageHistory(): void {
    // Keep only the most recent 50% of messages
    const keepCount = Math.floor(this.options.maxMessageHistory * 0.5);
    this.messageHistory.splice(0, this.messageHistory.length - keepCount);
  }

  clearExpiredCache(): void {
    // Remove old cache entries (implementation depends on cache structure)
    this.cache.clear();
  }

  emergencyCleanup(): void {
    // Keep only the most recent 10% of messages
    const keepCount = Math.floor(this.options.maxMessageHistory * 0.1);
    this.messageHistory.splice(0, this.messageHistory.length - keepCount);
    
    // Clear all cache
    this.cache.clear();
  }

  cleanup(): void {
    this.trimMessageHistory();
    this.clearExpiredCache();
  }
}
```

## 5. Production Deployment Example

### 5.1 Complete Production Setup

```typescript
// production-main.ts
import { createLogger } from 'winston';
import { QiAsyncMessageQueue } from '@qi/amsg';
import { MessageDrivenCLI } from '@qi/cli';
import { loadConfiguration } from './config';
import { createHealthCheckServer } from './health-check';
import { setupMetricsCollection } from './metrics';

async function main(): Promise<void> {
  const config = await loadConfiguration();
  
  // Set up logging
  const logger = createLogger({
    level: config.logging.level,
    format: winston.format.json(),
    transports: [
      new winston.transports.Console({
        format: winston.format.simple()
      }),
      new winston.transports.File({ 
        filename: 'cli-error.log', 
        level: 'error' 
      }),
      new winston.transports.File({ 
        filename: 'cli-combined.log' 
      })
    ]
  });

  try {
    // Create production CLI
    const cli = await createProductionCLI(config);
    
    // Set up health check endpoint
    const healthServer = createHealthCheckServer(cli, config.health.port);
    
    // Set up metrics collection
    if (config.metrics.enabled) {
      setupMetricsCollection(cli, config.metrics);
    }
    
    // Graceful shutdown handling
    setupGracefulShutdown(cli, healthServer, logger);
    
    // Start the CLI
    const startResult = await cli.start();
    
    if (!startResult.success) {
      logger.error('Failed to start CLI:', startResult.error);
      process.exit(1);
    }
    
    logger.info('🚀 Production CLI started successfully');
    
    // Keep the process alive
    process.stdin.resume();
    
  } catch (error) {
    logger.error('💥 Fatal error during startup:', error);
    process.exit(1);
  }
}

function setupGracefulShutdown(
  cli: MessageDrivenCLI,
  healthServer: HealthCheckServer,
  logger: Logger
): void {
  const shutdown = async (signal: string) => {
    logger.info(`🛑 Received ${signal}, shutting down gracefully...`);
    
    try {
      // Stop accepting new connections
      healthServer.close();
      
      // Stop CLI
      await cli.stop();
      
      logger.info('✅ Graceful shutdown completed');
      process.exit(0);
    } catch (error) {
      logger.error('❌ Error during shutdown:', error);
      process.exit(1);
    }
  };

  process.on('SIGTERM', () => shutdown('SIGTERM'));
  process.on('SIGINT', () => shutdown('SIGINT'));
  
  process.on('uncaughtException', (error) => {
    logger.error('💥 Uncaught exception:', error);
    shutdown('uncaughtException');
  });
  
  process.on('unhandledRejection', (reason, promise) => {
    logger.error('💥 Unhandled rejection at:', promise, 'reason:', reason);
    shutdown('unhandledRejection');
  });
}

// Start the application
main().catch(console.error);
```

This comprehensive integration guide provides complete working examples for implementing robust CLI applications with proper error handling, performance optimization, and production deployment patterns.