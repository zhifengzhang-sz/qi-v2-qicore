/**
 * v-0.6.1 Pure Enqueue-Only CLI Framework
 *
 * Transformed for pure message-driven architecture:
 * - No direct orchestrator calls - only enqueues messages
 * - Integrates with QiAsyncMessageQueue for coordination
 * - Maintains same public API for backward compatibility
 * - All processing happens through message flow
 *
 * Key changes for v-0.6.1:
 * - sendToAgent() -> enqueueUserInput()
 * - All agent communication through message queue
 * - Event emission converted to message enqueuing
 * - Pure display and input handling only
 */

// QiCore imports for v-0.8.1
import {
  create,
  failure,
  fromAsyncTryCatch,
  match,
  type QiError,
  type Result,
  success,
} from '@qi/base'
import type { ICommandHandler } from '../command/abstractions/index'
// v-0.6.1 Message Queue integration
import type { QiAsyncMessageQueue } from '@qi/amsg'
import type { QiMessage, UserInputMessage } from '@qi/amsg'
import { MessagePriority, MessageType } from '@qi/amsg'
import type {
  CLIConfig,
  CLIEvents,
  MessageType as CLIMessageType,
  CLIMode,
  CLIState,
  IAgentCLIBridge,
  ICLIFramework,
} from '../abstractions/ICLIFramework'
import type { ICommandRouter } from '../abstractions/ICLIServices'
import type { IInputManager } from '../abstractions/IInputManager'
// Injected dependencies interfaces
import type { ITerminal } from '../abstractions/ITerminal'
import type {
  IModeRenderer,
  IProgressRenderer,
  IStreamRenderer,
} from '../abstractions/IUIComponent'
// HotkeyManager for hotkey support
import { HotkeyManager } from '../keyboard/HotkeyManager'

/**
 * CLI-specific error factory functions using QiCore patterns
 */
const cliError = {
  initializationFailed: (reason: string): QiError =>
    create('CLI_INITIALIZATION_FAILED', `CLI initialization failed: ${reason}`, 'SYSTEM', {
      reason,
    }),

  shutdownFailed: (reason: string): QiError =>
    create('CLI_SHUTDOWN_FAILED', `CLI shutdown failed: ${reason}`, 'SYSTEM', { reason }),

  invalidInput: (input: string): QiError =>
    create('CLI_INVALID_INPUT', `Invalid CLI input: ${input}`, 'VALIDATION', { input }),

  messageEnqueueFailed: (messageId: string, reason: string): QiError =>
    create(
      'CLI_MESSAGE_ENQUEUE_FAILED',
      `Failed to enqueue message ${messageId}: ${reason}`,
      'SYSTEM',
      {
        messageId,
        reason,
      }
    ),

  configurationError: (field: string, value: unknown): QiError =>
    create('CLI_CONFIGURATION_ERROR', `Invalid configuration for ${field}`, 'VALIDATION', {
      field,
      value,
    }),

  componentError: (component: string, operation: string, reason: string): QiError =>
    create(
      'CLI_COMPONENT_ERROR',
      `Component ${component} failed during ${operation}: ${reason}`,
      'SYSTEM',
      {
        component,
        operation,
        reason,
      }
    ),

  stateError: (operation: string, currentState: string): QiError =>
    create(
      'CLI_STATE_ERROR',
      `Cannot perform ${operation} in state: ${currentState}`,
      'VALIDATION',
      {
        operation,
        currentState,
      }
    ),
}

/**
 * v-0.6.1 Pure Message-Driven CLI Architecture
 *
 * This implementation delegates all operations to injected components:
 * - Terminal operations → ITerminal implementation
 * - Input handling → IInputManager implementation
 * - Progress display → IProgressRenderer implementation
 * - Mode management → IModeRenderer implementation
 * - Streaming → IStreamRenderer implementation
 * - v-0.6.1: Events removed - pure message coordination
 * - Commands → ICommandRouter implementation
 * - Message coordination → QiAsyncMessageQueue (v-0.6.1)
 */
export class MessageDrivenCLI implements ICLIFramework, IAgentCLIBridge {
  private config: CLIConfig
  private state: CLIState
  private isInitialized = false
  private isStarted = false
  private hotkeyManager?: HotkeyManager

  constructor(
    private terminal: ITerminal,
    private inputManager: IInputManager,
    private progressRenderer: IProgressRenderer,
    private modeRenderer: IModeRenderer,
    private streamRenderer: IStreamRenderer,
    _commandRouter: ICommandRouter,
    private messageQueue: QiAsyncMessageQueue<QiMessage>, // v-0.6.1: Replace agentConnector
    config: Partial<CLIConfig> = {},
    _commandHandler?: ICommandHandler
  ) {
    // Default configuration
    this.config = {
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
      ...config,
    }

    // Initialize state
    this.state = {
      mode: 'interactive',
      isProcessing: false,
      isStreamingActive: false,
      currentInput: '',
      history: [],
      startTime: new Date(),
      lastActivity: new Date(),
    }

    // v-0.6.1: No event handlers needed - pure message-driven
  }

  /**
   * Initialize the CLI framework
   */
  /**
   * QiCore-compliant initialization
   */
  async initializeQiCore(): Promise<Result<void, QiError>> {
    if (this.isInitialized) {
      return success(undefined)
    }

    return fromAsyncTryCatch(
      async () => {
        // v-0.6.1: Minimal initialization - pure message-driven CLI
        this.inputManager.initialize({
          historySize: this.config.historySize,
          autoComplete: this.config.autoComplete,
          enableColors: this.config.colors,
        })

        // Essential: Connect input capture to message flow
        // NOTE: This is only used for readline-based frameworks
        // Ink/Hybrid frameworks handle input directly through React components
        this.inputManager.onInput((input) => {
          console.log(`[MessageDrivenCLI] inputManager.onInput called: "${input}"`)
          const handleResult = this.handleInputInternal(input)
          match(
            () => {}, // Success case - input handled
            (error) => console.error('Input handling error:', error),
            handleResult
          )
        })

        // Initialize HotkeyManager if hotkeys are enabled
        if (this.config.enableHotkeys) {
          this.hotkeyManager = new HotkeyManager(this.messageQueue, {
            enableShiftTab: true,
            enableEscape: true,
            enableCtrlC: true,
            passthrough: true,
          })

          console.log('[MessageDrivenCLI] HotkeyManager initialized')
        }

        this.isInitialized = true
        return undefined
      },
      (error) => cliError.initializationFailed(String(error))
    )
  }

  /**
   * Legacy interface method - maintains compatibility
   */
  async initialize(): Promise<void> {
    const result = await this.initializeQiCore()
    return match(
      () => {}, // Success - return void
      (error) => {
        console.error('CLI initialization failed:', error)
        throw new Error(error.message)
      },
      result
    )
  }

  /**
   * QiCore-compliant start method
   */
  async startQiCore(): Promise<Result<void, QiError>> {
    return fromAsyncTryCatch(
      async () => {
        if (!this.isInitialized) {
          const initResult = await this.initializeQiCore()
          return match(
            () => {}, // Continue with start
            (error) => {
              throw error
            }, // Propagate init error
            initResult
          )
        }

        if (this.isStarted) {
          return undefined
        }

        this.terminal.writeLine('🚀 Message-Driven CLI Ready')
        this.terminal.writeLine('============================')
        this.terminal.writeLine('💡 Type /exit to quit')
        if (this.config.enableHotkeys) {
          this.terminal.writeLine('⌨️  ESC: Cancel operation, Shift+Tab: Cycle modes')
        }
        this.terminal.writeLine('')

        // Enable hotkeys after startup
        if (this.hotkeyManager) {
          this.hotkeyManager.enable()
          console.log('[MessageDrivenCLI] HotkeyManager enabled')
        }

        // Show initial prompt
        this.inputManager.showPrompt()

        this.isStarted = true
        return undefined
      },
      (error) => cliError.initializationFailed(`Start failed: ${error}`)
    )
  }

  /**
   * Legacy interface method - maintains compatibility
   */
  async start(): Promise<void> {
    const result = await this.startQiCore()
    return match(
      () => {}, // Success - return void
      (error) => {
        console.error('CLI start failed:', error)
        throw new Error(error.message)
      },
      result
    )
  }

  /**
   * QiCore-compliant shutdown method
   */
  async shutdownQiCore(): Promise<Result<void, QiError>> {
    if (!this.isStarted) {
      return success(undefined)
    }

    return fromAsyncTryCatch(
      async () => {
        // Cancel any active operations
        if (this.state.isProcessing) {
          // v-0.6.1: Direct cancellation instead of events
          this.state.isProcessing = false
        }

        // Cleanup components
        this.progressRenderer.destroy()
        this.modeRenderer.destroy()
        this.streamRenderer.destroy()
        this.inputManager.close()

        // Cleanup HotkeyManager
        if (this.hotkeyManager) {
          this.hotkeyManager.destroy()
          console.log('[MessageDrivenCLI] HotkeyManager destroyed')
        }
        // v-0.6.1: No eventManager to destroy

        this.isStarted = false
        return undefined
        // v-0.6.1: No shutdown events
      },
      (error) => cliError.shutdownFailed(String(error))
    )
  }

  /**
   * Legacy interface method - maintains compatibility
   */
  async shutdown(): Promise<void> {
    const result = await this.shutdownQiCore()
    return match(
      () => {}, // Success - return void
      (error) => {
        console.error('CLI shutdown failed:', error)
        throw new Error(error.message)
      },
      result
    )
  }

  // State management (delegated to components)

  getState(): CLIState {
    return { ...this.state }
  }

  setMode(mode: CLIMode): void {
    // v-0.6.1: Simplified mode setting with QiCore validation
    const result = this.setModeInternal(mode)
    match(
      () => {}, // Success - return void
      (error) => {
        console.error('Set mode error:', error)
        // Don't change mode on error
      },
      result
    )
  }

  private setModeInternal(mode: CLIMode): Result<void> {
    if (!mode || !['interactive', 'command', 'streaming'].includes(mode)) {
      return failure(cliError.configurationError('mode', mode))
    }
    this.state.mode = mode
    return success(undefined)
  }

  getMode(): CLIMode {
    return this.state.mode
  }

  // Input/Output (delegated to injected components)

  showPrompt(): void {
    const result = this.showPromptInternal()
    match(
      () => {}, // Success - return void
      (error) => console.error('Show prompt error:', error),
      result
    )
  }

  private showPromptInternal(): Result<void> {
    try {
      if (this.isPromptActive()) {
        this.inputManager.showPrompt()
      }
      return success(undefined)
    } catch (error) {
      return failure(cliError.componentError('inputManager', 'showPrompt', String(error)))
    }
  }

  private isPromptActive(): boolean {
    return !this.state.isProcessing && !this.state.isStreamingActive
  }

  // v-0.6.1: handleInput moved to line 351 to follow design specification

  displayMessage(content: string, _type?: CLIMessageType): void {
    const result = this.displayMessageInternal(content, _type)
    match(
      () => {}, // Success - return void
      (error) => console.error('Display message error:', error),
      result
    )
  }

  private displayMessageInternal(content: string, _type?: CLIMessageType): Result<void> {
    try {
      // Only responsibility: display (EXACT design specification)
      this.terminal.writeLine(content)

      // CRITICAL: Reset processing state when displaying final message
      this.state.isProcessing = false

      // Essential: Show prompt for next input
      this.inputManager.showPrompt()
      return success(undefined)
    } catch (error) {
      return failure(cliError.componentError('terminal', 'displayMessage', String(error)))
    }
  }

  /**
   * CRITICAL: Reset processing state to stop infinite loading
   * Called by QiPromptCLI after processing messages
   */
  resetProcessingState(): void {
    this.state.isProcessing = false
  }

  displayProgress(phase: string, progress: number, details?: string): void {
    const result = this.displayProgressInternal(phase, progress, details)
    match(
      () => {}, // Success - return void
      (error) => console.error('Display progress error:', error),
      result
    )
  }

  private displayProgressInternal(phase: string, progress: number, details?: string): Result<void> {
    try {
      if (progress < 0 || progress > 100) {
        return failure(cliError.configurationError('progress', progress))
      }
      this.progressRenderer.updateProgress(progress, phase, details)
      return success(undefined)
    } catch (error) {
      return failure(cliError.componentError('progressRenderer', 'displayProgress', String(error)))
    }
  }

  // Streaming methods (delegated to streamRenderer)
  startStreaming(): void {
    const result = this.startStreamingInternal()
    match(
      () => {}, // Success - return void
      (error) => console.error('Start streaming error:', error),
      result
    )
  }

  private startStreamingInternal(): Result<void> {
    try {
      if (this.state.isStreamingActive) {
        return failure(cliError.stateError('startStreaming', 'already streaming'))
      }
      this.state.isStreamingActive = true
      this.streamRenderer.startStreaming()
      return success(undefined)
    } catch (error) {
      return failure(cliError.componentError('streamRenderer', 'startStreaming', String(error)))
    }
  }

  addStreamingChunk(content: string): void {
    const result = this.addStreamingChunkInternal(content)
    match(
      () => {}, // Success - return void
      (error) => console.error('Add streaming chunk error:', error),
      result
    )
  }

  private addStreamingChunkInternal(content: string): Result<void> {
    try {
      if (!this.state.isStreamingActive) {
        return failure(cliError.stateError('addStreamingChunk', 'not streaming'))
      }
      this.streamRenderer.addChunk(content)
      return success(undefined)
    } catch (error) {
      return failure(cliError.componentError('streamRenderer', 'addStreamingChunk', String(error)))
    }
  }

  completeStreaming(message?: string): void {
    const result = this.completeStreamingInternal(message)
    match(
      () => {}, // Success - return void
      (error) => console.error('Complete streaming error:', error),
      result
    )
  }

  private completeStreamingInternal(message?: string): Result<void> {
    try {
      this.state.isStreamingActive = false
      this.streamRenderer.complete(message)
      const promptResult = this.showPromptInternal()
      return match(
        (): Result<void> => success(undefined),
        (error: QiError): Result<void> => failure(error),
        promptResult
      )
    } catch (error) {
      return failure(cliError.componentError('streamRenderer', 'completeStreaming', String(error)))
    }
  }

  cancelStreaming(): void {
    const result = this.cancelStreamingInternal()
    match(
      () => {}, // Success - return void
      (error) => console.error('Cancel streaming error:', error),
      result
    )
  }

  private cancelStreamingInternal(): Result<void> {
    try {
      this.state.isStreamingActive = false
      this.streamRenderer.cancel()
      const promptResult = this.showPromptInternal()
      return match(
        (): Result<void> => success(undefined),
        (error: QiError): Result<void> => failure(error),
        promptResult
      )
    } catch (error) {
      return failure(cliError.componentError('streamRenderer', 'cancelStreaming', String(error)))
    }
  }

  // Event methods (deprecated for v-0.6.1 message-driven architecture)
  on<K extends keyof CLIEvents>(event: K, _listener: (data: CLIEvents[K]) => void): void {
    // v-0.6.1: Event system simplified - most events converted to messages
    console.warn(
      `[MessageDrivenCLI] Event '${event}' registration - consider using message queue instead`
    )
  }

  off<K extends keyof CLIEvents>(event: K, _listener: (data: CLIEvents[K]) => void): void {
    // v-0.6.1: Event system simplified
    console.warn(
      `[MessageDrivenCLI] Event '${event}' removal - consider using message queue instead`
    )
  }

  emit<K extends keyof CLIEvents>(event: K, _data: CLIEvents[K]): void {
    // v-0.6.1: Most events converted to messages
    console.warn(
      `[MessageDrivenCLI] Event '${event}' emission - consider using message queue instead`
    )
  }

  // IAgentCLIBridge implementation (simplified for v-0.6.1)
  connectAgent(_agent: any): void {
    // v-0.6.1: Agent connection handled through message queue
    console.log('[MessageDrivenCLI] Agent connection - handled via message queue')
  }

  disconnectAgent(): void {
    // v-0.6.1: Agent disconnection handled through message queue
    console.log('[MessageDrivenCLI] Agent disconnection - handled via message queue')
  }

  onAgentProgress(progress: { phase: string; progress: number; details?: string }): void {
    this.displayProgress(progress.phase, progress.progress, progress.details)
  }

  onAgentMessage(message: { content: string; type: string }): void {
    this.displayMessage(message.content, message.type as CLIMessageType)
  }

  onAgentComplete(result: any): void {
    this.state.isProcessing = false
    this.displayMessage(`✅ Complete: ${result}`)
  }

  onAgentError(error: any): void {
    this.state.isProcessing = false
    this.displayMessage(`❌ Error: ${error}`, 'error')
  }

  onAgentCancelled(reason: string): void {
    this.state.isProcessing = false
    this.displayMessage(`🛑 Cancelled: ${reason}`, 'warning')
  }

  sendToAgent(input: string): void {
    // v-0.6.1: Delegate to handleInput which uses message queue
    this.handleInput(input)
  }

  cancelAgent(): void {
    this.state.isProcessing = false
    this.cancelStreaming()
    this.displayMessage('🛑 Operation cancelled')
  }

  // v-0.6.1: All complex methods removed - CLI only enqueues and displays

  // v-0.6.1: Event handling completely removed - pure message-driven architecture

  // Configuration

  // State Management Integration (required by ICLIFramework)
  subscribeToStateChanges(_stateManager: any): void {
    // v-0.6.1: MessageDrivenCLI uses message queue for state coordination
    console.log('[MessageDrivenCLI] StateManager subscription - handled via message queue')
  }

  updateConfig(newConfig: Partial<CLIConfig>): void {
    const result = this.updateConfigInternal(newConfig)
    match(
      () => {}, // Success - return void
      (error) => console.error('Update config error:', error),
      result
    )
  }

  private updateConfigInternal(newConfig: Partial<CLIConfig>): Result<void> {
    try {
      if (!newConfig || typeof newConfig !== 'object') {
        return failure(cliError.configurationError('config', newConfig))
      }

      this.config = { ...this.config, ...newConfig }

      // Update component configurations
      this.progressRenderer.updateConfig({ animated: this.config.animations })
      this.streamRenderer.updateConfig({ throttleMs: this.config.streamingThrottle })
      this.inputManager.updateConfig({
        historySize: this.config.historySize,
        autoComplete: this.config.autoComplete,
      })

      return success(undefined)
    } catch (error) {
      return failure(cliError.configurationError('updateConfig', String(error)))
    }
  }

  getConfig(): CLIConfig {
    return { ...this.config }
  }

  // v-0.6.1: Pure message queue - no agent integration methods needed

  // v-0.6.1: Agent callbacks removed - communication through message queue only

  // Public interface method - maintains compatibility
  handleInput(input: string): void {
    const result = this.handleInputInternal(input)
    match(
      () => {}, // Success - return void
      (error) => {
        console.error('Input handling error:', error)
        // Don't throw, just log error to maintain CLI stability
      },
      result
    )
  }

  // Internal QiCore implementation
  private handleInputInternal(input: string): Result<void> {
    console.log(`[MessageDrivenCLI] handleInput called with: "${input}"`)

    // Validate input with pure function
    const validateInput = (userInput: string): Result<string> => {
      if (typeof userInput !== 'string') {
        return failure(cliError.invalidInput('Input must be a string'))
      }
      return success(userInput.trim())
    }

    // Handle essential commands with functional pattern
    const handleSystemCommands = (trimmedInput: string): Result<'exit' | 'message'> => {
      if (trimmedInput === '/exit' || trimmedInput === '/quit') {
        try {
          this.terminal.writeLine('👋 Goodbye!')
          process.exit(0)
          return success('exit')
        } catch (error) {
          return failure(cliError.componentError('terminal', 'exit', String(error)))
        }
      }
      return success('message')
    }

    // Create and enqueue user message
    const createAndEnqueueMessage = (originalInput: string): Result<void> => {
      const messageId = this.generateMessageId()
      const userInputMessage: UserInputMessage = {
        id: messageId,
        type: MessageType.USER_INPUT,
        timestamp: new Date(),
        priority: MessagePriority.NORMAL,
        input: originalInput,
        raw: false,
        source: 'cli',
      }

      console.log(`[MessageDrivenCLI] Enqueuing to message queue`)
      const enqueueResult = this.messageQueue.enqueue(userInputMessage)

      return match(
        (): Result<void> => {
          this.state.isProcessing = true
          return success(undefined)
        },
        (error: QiError): Result<void> =>
          failure(cliError.messageEnqueueFailed(messageId, error.message)),
        enqueueResult
      )
    }

    // Functional composition chain
    const validationResult = validateInput(input)
    return match(
      (trimmedInput) => {
        const commandResult = handleSystemCommands(trimmedInput)
        return match(
          (commandType) => {
            if (commandType === 'exit') {
              return success(undefined) // Exit handled
            }
            return createAndEnqueueMessage(input) // Process as message
          },
          (error) => failure(error),
          commandResult
        )
      },
      (error) => failure(error),
      validationResult
    )
  }

  // Helper function for message ID generation
  private generateMessageId(): string {
    return Math.random().toString(36).substring(2, 15)
  }

  // v-0.6.1: Pure message-driven CLI - only handleInput and displayMessage

  // v-0.6.1: All complex private methods removed - pure message-driven only

  // v-0.6.1: All complex private methods removed - CLI is now pure (only enqueue and display)
}

/**
 * v-0.6.1 Factory function - requires message queue injection
 */
export function createMessageDrivenCLI(
  _messageQueue: QiAsyncMessageQueue<QiMessage>,
  _config?: Partial<CLIConfig>
): MessageDrivenCLI {
  // v-0.6.1: Message queue is required for pure enqueue-only architecture
  // The actual implementation will be handled by the factory functions
  throw new Error('Use createCLI() from factories with messageQueue parameter instead')
}
