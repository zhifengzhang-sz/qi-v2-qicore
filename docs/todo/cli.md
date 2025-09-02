# QiCore CLI Framework TODOs

## Current Status Assessment (January 2025)
✅ **COMPLETED**: 
- TypeScript build system with modern tsup configuration
- Core CLI framework architecture (`MessageDrivenCLI`)
- State management system (`CLIStateManager` with XState 5)
- Async messaging integration (`@qi/amsg` with `QiAsyncMessageQueue`)
- Command system foundation (`SimpleCLICommandHandler`)
- Multiple UI frameworks (Ink, Readline, Hybrid)
- Input handling and keyboard management

❌ **MISSING**: 
- Advanced command registration for applications
- External state management patterns (provider/model coordination)
- Production application examples (qi-prompt, qi-code)
- AgentChat/AutoGen integration patterns

## Required Enhancements (Not Missing Core Components)

### 1. Application Command Registration System

#### 1.1 Dynamic Command Registration API
**Status**: Foundation exists, needs enhancement for dynamic registration

✅ **Current**: `SimpleCLICommandHandler` with basic command support
❌ **Needed**: Dynamic registration for application-specific commands

- [ ] **Enhanced Command Registration**
  ```typescript
  interface AppCommandRegistry {
    registerCommand(definition: AppCommandDefinition): Result<void, QiError>
    unregisterCommand(name: string): Result<void, QiError>
    listCommands(): CommandDefinition[]
    getCommand(name: string): Result<CommandDefinition, QiError>
  }

  interface AppCommandDefinition {
    name: string
    description: string
    args: ArgumentDefinition[]
    flags: FlagDefinition[]
    handler: (request: CommandRequest) => Promise<Result<CommandResponse, QiError>>
    scope: 'app' | 'global'
    requiresState?: string[]
  }
  ```

- [ ] **Command Discovery and Loading**
  - Auto-discovery from app modules
  - Plugin-based command loading  
  - Command namespace management
  - Version compatibility checking

#### 1.2 Advanced Message Integration
**Status**: Basic integration exists, needs application-specific patterns

✅ **Current**: `MessageDrivenCLI` integrates with `QiAsyncMessageQueue`
❌ **Needed**: Application-specific message patterns

- [ ] **Application Message Types**
  ```typescript
  // Extend existing QiMessage for app-specific needs
  interface AppSpecificMessage extends QiMessage {
    appId: string
    commandId?: string
    stateSync?: boolean
  }
  ```

### 2. External State Management Coordination

#### 2.1 Provider/Model State Architecture  
**Status**: Core CLI state management exists, external coordination needed

✅ **Current**: `CLIStateManager` with XState 5 manages internal CLI state
❌ **Needed**: Coordination with external application state (providers, models)

**CRITICAL DECISION**: State ownership architecture
```typescript
// Option A: CLI as State Coordinator
interface CLIExternalStateManager {
  currentProvider: string
  currentModel: string
  syncWithApp(): Promise<Result<void, QiError>>
  notifyStateChange(change: StateChange): Promise<void>
}

// Option B: App-Owned State, CLI Consumer  
interface AppStateSubscription {
  subscribeToProviderChanges(callback: (provider: string) => void): void
  subscribeToModelChanges(callback: (model: string) => void): void
  getCurrentState(): { provider: string, model: string }
}
```

- [ ] **State Synchronization Pattern**
  - Define ownership boundaries
  - Implement change notification system
  - Handle state conflicts and resolution
  - Ensure consistency across CLI and app

#### 2.2 Command-State Integration
**Status**: Basic command handling exists, state-aware commands needed

- [ ] **State-Dependent Commands**
  ```typescript
  // Commands that need external state
  interface StateAwareCommand {
    name: string
    requiredState: string[]  // ['provider', 'model', 'connection']
    handler: (request: CommandRequest, state: ExternalState) => Promise<Result<CommandResponse, QiError>>
  }
  ```

### 3. Production Integration Patterns

#### 3.1 Application Integration Templates
**Status**: CLI foundation complete, need integration examples

✅ **Current**: Complete CLI framework ready for integration
❌ **Needed**: Concrete examples and patterns for real applications

- [ ] **Integration Template Creation**
  ```typescript
  // Template for applications to integrate with CLI
  interface QiCLIIntegration {
    createApp(appConfig: AppConfig): Promise<Result<QiApp, QiError>>
    registerCommands(cli: MessageDrivenCLI, app: QiApp): Promise<Result<void, QiError>>
    startWithCLI(app: QiApp): Promise<Result<void, QiError>>
  }
  ```

- [ ] **Application Lifecycle Management**
  - App initialization with CLI integration
  - Command registration during startup
  - Graceful shutdown coordination
  - Error recovery and restart patterns

#### 3.2 Advanced UI Component Integration
**Status**: Multiple frameworks supported, need advanced components

✅ **Current**: Ink, Readline, Hybrid frameworks implemented
❌ **Needed**: Advanced UI components for specific use cases

- [ ] **Specialized UI Components**
  - Real-time conversation display
  - Multi-agent status indicators  
  - Code highlighting and display
  - Progress tracking for long operations
  - Interactive menus and selection

## Implementation Priority (Revised)

### Phase 1: Application Integration (Highest Priority)
**Goal**: Enable real applications to use the existing CLI framework

1. **Dynamic Command Registration System**
   - Enhance `SimpleCLICommandHandler` for app-specific commands
   - Create registration API for applications
   - Implement command discovery and loading

2. **External State Coordination** 
   - Decide on state ownership architecture (CLI vs App vs Shared)
   - Implement state synchronization patterns
   - Create state-aware command execution

3. **Integration Templates and Examples**
   - Create application integration patterns
   - Document best practices
   - Provide starter templates

### Phase 2: Advanced UI Components (Medium Priority)
**Goal**: Enhance the existing UI frameworks for specific use cases

1. **Specialized Components**
   - Conversation display components
   - Multi-agent status displays
   - Code syntax highlighting
   - Interactive selection menus

2. **Framework Enhancements**
   - Advanced Ink components
   - Enhanced Readline features
   - Improved Hybrid mode switching

### Phase 3: Production Features (Lower Priority) 
**Goal**: Production-ready features and optimizations

1. **Advanced Features**
   - Plugin system for commands
   - Advanced configuration management
   - Performance monitoring and metrics
   - Comprehensive error recovery

## Implementation Priority (Revised)

### Phase 1: Application Integration (Highest Priority)  
**Goal**: Enable real applications to use the existing CLI framework

1. **Dynamic Command Registration System**
   - Enhance `SimpleCLICommandHandler` for app-specific commands
   - Create registration API for applications
   - Implement command discovery and loading

2. **External State Coordination** 
   - Decide on state ownership architecture (CLI vs App vs Shared)
   - Implement state synchronization patterns
   - Create state-aware command execution

3. **Integration Templates and Examples**
   - Create application integration patterns
   - Document best practices
   - Provide starter templates

### Phase 2: Advanced UI Components (Medium Priority)
**Goal**: Enhance the existing UI frameworks for specific use cases

1. **Specialized Components**
   - Conversation display components
   - Multi-agent status displays
   - Code syntax highlighting
   - Interactive selection menus

2. **Framework Enhancements**
   - Advanced Ink components
   - Enhanced Readline features
   - Improved Hybrid mode switching

### Phase 3: Production Features (Lower Priority)
**Goal**: Production-ready features and optimizations

1. **Advanced Features**
   - Plugin system for commands
   - Advanced configuration management
   - Performance monitoring and metrics
   - Comprehensive error recovery

## Critical Questions Needing Answers

1. **State Management Architecture**: Who owns provider/model state?
2. **App Integration Pattern**: How do apps register commands?
3. **Message Routing**: How do commands communicate with external modules?
4. **Error Handling**: How to handle cross-module errors?
5. **State Synchronization**: How to keep CLI and app state in sync?

## Success Criteria

A complete CLI framework should enable:

```typescript
// App developer can easily integrate
const cli = new QiCLI({
  framework: 'hybrid',
  stateManager: appStateManager,
  messageQueue: appMessageQueue
})

// Register app-specific commands
cli.registerCommand({
  name: 'model',
  handler: async (args) => await app.changeModel(args.model)
})

// Start CLI with full functionality  
await cli.start()
```

This CLI should power both qi-prompt and qi-code applications seamlessly.