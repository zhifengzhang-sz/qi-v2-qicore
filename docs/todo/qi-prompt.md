# qi-prompt Application TODOs

## Vision
**qi-prompt** = Advanced conversational AI assistant with QiCore CLI + AgentChat integration

A sophisticated prompt-based application that provides intelligent conversation, task assistance, and knowledge work through a modern terminal interface.

## Current Status Assessment (January 2025)
✅ **FOUNDATION READY**: 
- QiCore CLI framework (`MessageDrivenCLI`) with complete implementation
- State management system (`CLIStateManager` with XState 5)
- Async messaging integration (`@qi/amsg`)
- Multi-framework UI support (Ink, Readline, Hybrid)
- Command system foundation (`SimpleCLICommandHandler`)

❌ **MISSING**: 
- AgentChat integration patterns and bridge layer
- Application-specific command registration for qi-prompt
- LLM provider management and conversation state
- Specialized agent implementations

## Architecture Overview

### Core Stack
- **@qi/cli** - Terminal interface and command system
- **@qi/amsg** - Async messaging between components  
- **@qi/base + @qi/core** - Functional foundation
- **AgentChat** - Conversational AI engine (needs integration)
- **LLM Providers** - OpenAI, Anthropic, Ollama, etc.

### Application Structure
```
qi-prompt/
├── src/
│   ├── agents/          # AgentChat specialized agents
│   ├── providers/       # LLM provider integrations
│   ├── conversation/    # Conversation management
│   ├── cli/            # CLI integration
│   ├── state/          # Application state management
│   └── main.ts         # Application entry point
├── config/             # Configuration files
└── docs/               # Application documentation
```

## Critical TODOs

### 1. AgentChat Integration

#### 1.1 AgentChat Architecture Design
- [ ] **Research AgentChat Framework**
  - What is the current AgentChat API?
  - How does AgentChat handle conversations?
  - What are AgentChat's extension points?
  - How to create specialized agents?

- [ ] **Define AgentChat-QiCore Bridge**
  ```typescript
  interface QiAgentChat {
    createAgent(config: AgentConfig): Result<Agent, QiError>
    startConversation(agent: Agent): Result<Conversation, QiError>  
    sendMessage(conversation: Conversation, message: string): Promise<Result<AgentResponse, QiError>>
    getConversationHistory(conversation: Conversation): ConversationHistory[]
  }
  ```

#### 1.2 Specialized Prompt Agent
- [ ] **Design Prompt-Optimized Agent**
  ```typescript
  interface PromptAgent extends Agent {
    type: 'prompt_assistant'
    capabilities: {
      longFormResponses: boolean
      codeGeneration: boolean  
      knowledgeWork: boolean
      creativeWriting: boolean
      analysis: boolean
    }
    personality: {
      tone: 'professional' | 'casual' | 'technical'
      verbosity: 'concise' | 'detailed' | 'comprehensive'
      expertise: string[]
    }
  }
  ```

- [ ] **Agent Specialization System**
  - General conversation agent (default)
  - Technical writing agent
  - Code explanation agent  
  - Creative writing agent
  - Analysis and research agent
  - Domain-specific expert agents

### 2. LLM Provider Management

#### 2.1 Multi-Provider Architecture
- [ ] **Provider Abstraction Layer**
  ```typescript
  interface LLMProvider {
    name: string
    models: LLMModel[]
    capabilities: ProviderCapabilities
    authenticate(credentials: ProviderCredentials): Promise<Result<void, QiError>>
    generateResponse(prompt: string, config: GenerationConfig): Promise<Result<LLMResponse, QiError>>
  }
  ```

- [ ] **Supported Providers**
  - OpenAI (GPT-4, GPT-3.5)
  - Anthropic (Claude 3.5 Sonnet, Claude 3 Haiku)
  - Ollama (Local models)
  - Google (Gemini Pro)
  - Others as needed

#### 2.2 Model Management
- [ ] **Model Selection System**
  ```typescript
  interface ModelManager {
    listAvailableModels(): LLMModel[]
    getCurrentModel(): LLMModel
    switchModel(provider: string, model: string): Promise<Result<void, QiError>>
    validateModelAccess(model: LLMModel): Promise<Result<boolean, QiError>>
  }
  ```

- [ ] **Model Configuration**
  - Temperature, top_p, max_tokens per model
  - Model-specific prompt formatting
  - Cost tracking and limits
  - Performance monitoring

### 3. Conversation Management

#### 3.1 Conversation Engine
- [ ] **Conversation State Management**
  ```typescript
  interface ConversationState {
    id: string
    agent: PromptAgent
    model: LLMModel
    messages: ConversationMessage[]
    context: ConversationContext
    metadata: ConversationMetadata
  }
  ```

- [ ] **Message Management**
  - Message history and persistence
  - Context window management  
  - Message threading and branching
  - Conversation search and retrieval

#### 3.2 Advanced Conversation Features
- [ ] **Context Management**
  - Automatic context summarization
  - Context injection and extraction
  - Long conversation handling
  - Context-aware responses

- [ ] **Conversation Tools**
  - Export conversations (markdown, JSON, PDF)
  - Import previous conversations
  - Conversation templates and prompts
  - Conversation analytics

### 4. CLI Integration & Commands

#### 4.1 Application Command Registration
**Status**: CLI framework exists, needs application-specific command integration

✅ **Current**: `SimpleCLICommandHandler` provides basic CLI commands (/help, /exit, /clear, /version)
❌ **Needed**: Application-specific command registration for qi-prompt functionality

- [ ] **Extend CLI Command System**
  ```typescript
  // Register qi-prompt commands with existing CLI framework
  const cli = new MessageDrivenCLI({
    framework: 'hybrid',
    stateManager: createStateManager(),
    messageQueue: new QiAsyncMessageQueue()
  })
  
  // Register application-specific commands
  cli.registerAppCommands(qiPromptCommands)
  ```

#### 4.2 qi-prompt Specific Commands
- [ ] **Conversation Commands**
  ```bash
  /new [agent_type]           # Start new conversation
  /load <conversation_id>     # Load previous conversation
  /save [name]               # Save current conversation
  /export <format>           # Export conversation
  /clear                     # Clear current conversation
  ```

- [ ] **Agent Commands**
  ```bash
  /agent list                # List available agents
  /agent switch <type>       # Switch agent type
  /agent config <setting>    # Configure current agent
  /agent create <name>       # Create custom agent
  ```

- [ ] **Model Commands**
  ```bash
  /model list               # List available models
  /model switch <provider> <model>  # Switch LLM model
  /model config <setting>   # Configure model parameters
  /model status            # Show current model info
  ```

#### 4.3 State Management Integration 
**Status**: CLI state system exists, needs coordination with application state

✅ **Current**: `CLIStateManager` manages internal CLI state with XState 5
❌ **Needed**: External application state coordination

- [ ] **Application State Coordination**
  ```typescript
  // Coordinate CLI state with qi-prompt application state
  interface QiPromptState {
    // CLI provides this layer
    cliState: AppStateContext
    
    // Application manages this layer  
    currentConversation?: ConversationState
    currentAgent: PromptAgent
    currentModel: LLMModel
    userPreferences: UserPreferences
    conversationHistory: ConversationSummary[]
  }
  ```

- [ ] **State Synchronization**
  - Coordinate CLI and application state changes
  - Handle state-dependent command execution
  - Persist application state across sessions
  - Sync provider/model changes between CLI and app

### 5. User Experience Features

#### 5.1 Advanced Terminal UI
- [ ] **Interactive Elements**
  - Real-time typing indicators
  - Streaming response display  
  - Conversation history browser
  - Agent/model status display

- [ ] **Visual Enhancements**
  - Syntax highlighting for code
  - Markdown rendering in terminal
  - Color-coded message types
  - Progress indicators

#### 5.2 Productivity Features
- [ ] **Smart Features**
  - Auto-completion for commands
  - Conversation templates
  - Quick prompt shortcuts
  - Context suggestions

- [ ] **Integration Features**
  - File input/output
  - Clipboard integration
  - External tool integration
  - API connectivity

### 6. Configuration & Customization

#### 6.1 Configuration System
- [ ] **Configuration Files**
  ```typescript
  interface QiPromptConfig {
    defaultAgent: string
    defaultModel: LLMModel
    providers: ProviderConfig[]
    ui: UIConfig
    conversation: ConversationConfig
  }
  ```

- [ ] **User Customization**
  - Custom agent definitions
  - Personal prompt templates
  - UI theme customization
  - Keyboard shortcuts

## Implementation Phases (Revised)

### Phase 1: CLI Integration Foundation (2-3 weeks)
**Goal**: Integrate existing CLI framework with qi-prompt architecture

1. **Application Integration Pattern**
   - Create qi-prompt application using `MessageDrivenCLI`
   - Implement application-specific command registration
   - Establish state coordination between CLI and application

2. **Basic AgentChat Bridge**
   - Research AgentChat integration patterns
   - Create QiCore-AgentChat bridge layer
   - Implement basic conversation management

3. **Single LLM Provider**
   - Implement provider abstraction using Result<T> patterns  
   - Add OpenAI or Ollama provider support
   - Basic model switching functionality

### Phase 2: Core Functionality (3-4 weeks)
**Goal**: Complete essential qi-prompt features

1. **Multi-Provider Architecture**
   - Provider abstraction layer with Result<T> error handling
   - Multiple LLM provider support (OpenAI, Anthropic, Ollama)
   - Model management and switching
   - Configuration system integration

2. **Conversation Management**
   - Conversation state management
   - Message history and persistence
   - Context management and summarization

### Phase 3: Advanced Features (4-5 weeks)
**Goal**: Specialized functionality and enhanced UX

1. **Agent Specialization System**
   - Multiple agent types (technical, creative, analysis)
   - Agent switching and configuration
   - Specialized prompt handling

2. **Enhanced UI Components**
   - Real-time conversation display
   - Agent/model status indicators
   - Syntax highlighting and markdown rendering
   - Interactive command menus

### Phase 4: Production Readiness (2-3 weeks)
**Goal**: Polish and deployment preparation

1. **Production Features**
   - Comprehensive error handling and recovery
   - Performance optimization
   - Configuration validation and management
   - Testing and documentation

## Critical Architecture Questions (Updated)

### 1. CLI Integration Strategy
- **How to register application-specific commands with existing CLI framework?**
- **How to coordinate CLI state with qi-prompt application state?**
- **How to handle application commands through MessageDrivenCLI?**

### 2. AgentChat Integration Strategy  
- **How to integrate AgentChat with QiCore functional patterns?**
- **Should AgentChat operations be wrapped in Result<T> patterns?**
- **How to handle AgentChat async operations with @qi/amsg?**

### 3. State Management Approach
- **Who owns provider/model state - CLI or application?**
- **How to synchronize conversation state with CLI state manager?**
- **Where to persist conversation data and application preferences?**

### 4. Provider Management Architecture
- **How to implement provider abstraction with Result<T> error handling?**
- **Provider authentication and credential storage strategy?**
- **Provider failover and error handling patterns?**

## Success Criteria

A complete qi-prompt application should provide:

```bash
# Launch qi-prompt
$ qi-prompt

🤖 qi-prompt v1.0.0 - Intelligent Conversation Assistant
Using: Claude 3.5 Sonnet (Anthropic) | Agent: General Assistant

> Hello! I need help writing a technical blog post about microservices.

[Agent responds with detailed guidance...]

> /agent switch technical_writer
✅ Switched to Technical Writing Agent

> Can you help me create an outline?

[Specialized technical writing response...]

> /model switch openai gpt-4
✅ Switched to GPT-4 (OpenAI)

> /save blog_post_conversation
✅ Conversation saved as 'blog_post_conversation'
```

The application should feel natural, powerful, and seamlessly integrate AI assistance into the user's workflow.