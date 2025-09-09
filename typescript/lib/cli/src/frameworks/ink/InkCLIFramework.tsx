/**
 * Ink CLI Framework Implementation
 *
 * React-based CLI implementation using Ink framework
 */

import { useState, useEffect, useCallback } from 'react'
import { render, useInput, useApp } from 'ink'
import { randomBytes } from 'crypto'
import { create, failure, success, fromTryCatch, type QiError, type Result } from '@qi/base'
import type {
  ICLIFramework,
  IAgentCLIBridge,
  CLIConfig,
  CLIState,
  CLIMode,
  MessageType as ICLIFrameworkMessageType,
} from '../../abstractions/ICLIFramework'
import type { QiAsyncMessageQueue } from '@qi/amsg'
import type { QiMessage } from '@qi/amsg'
import { MessageType } from '@qi/amsg'
import { HotkeyManager } from '../../keyboard/HotkeyManager'
import { MainLayout } from './components/MainLayout'
import {
  createUserMessage,
  createAssistantMessage,
  createSystemMessage,
  type OutputMessage,
} from './components/OutputDisplay'
import { createPermissionRequest, type PermissionRequest } from './components/PermissionDialog'
import { createDebugLogger } from '../../utils/DebugLogger'
import { createConditionalLogger, type SimpleLogger } from '../../utils/QiCoreLogger'

/**
 * CLI Framework error types
 */
interface InkFrameworkError extends QiError {
  context: {
    framework?: string
    operation?: string
    renderInstance?: boolean
  }
}

const inkFrameworkError = (
  code: string,
  message: string,
  context: InkFrameworkError['context'] = {}
): InkFrameworkError => create(code, message, 'SYSTEM', context) as InkFrameworkError

/**
 * Type-safe access to framework internals
 */
interface FrameworkWithInternals {
  config?: CLIConfig & { stateManager?: any }
  stateManager?: any
  debugMode?: boolean
  connectedAgent?: any
}

/**
 * Generate a unique message ID using crypto random bytes
 */
const generateUniqueId = (): string => {
  return randomBytes(8).toString('hex')
}

/**
 * Default CLI configuration for Ink
 */
const defaultInkConfig: CLIConfig = {
  enableHotkeys: true,
  enableModeIndicator: true,
  enableProgressDisplay: true,
  enableStreaming: true,
  prompt: '> ',
  colors: true,
  animations: true,
  historySize: 100,
  autoComplete: false,
  streamingThrottle: 10,
  maxBufferSize: 1000000,
  debug: false,
}

/**
 * Ink CLI Framework - React-based CLI implementation
 * v-0.6.1 Pure message-driven approach: All communication through MessageQueue
 */
export class InkCLIFramework implements ICLIFramework, IAgentCLIBridge {
  private config: CLIConfig
  private state: CLIState
  private isInitialized = false
  private isShutdown = false
  private renderInstance: any = null
  private connectedAgent: any = null
  private isCancelling = false
  private messageQueue?: QiAsyncMessageQueue<QiMessage>
  private debug = createDebugLogger('InkCLIFramework')
  private stateManager: any = null
  private stateUnsubscribe: (() => void) | null = null
  private setMessages: ((updater: (prev: OutputMessage[]) => OutputMessage[]) => void) | null = null
  private debugMode = false
  protected logger: SimpleLogger
  private hotkeyManager?: HotkeyManager

  constructor(config: Partial<CLIConfig> = {}, messageQueue?: QiAsyncMessageQueue<QiMessage>) {
    this.messageQueue = messageQueue

    // Store debug mode from config
    this.debugMode = (config as CLIConfig & { debug?: boolean }).debug || false

    // Initialize QiCore conditional logger
    this.logger = createConditionalLogger({
      level: 'info',
      name: 'InkCLIFramework',
      pretty: true,
      debugMode: this.debugMode,
    })

    this.logger.debug('🔍 InkCLIFramework constructor', undefined, {
      component: 'InkCLIFramework',
      method: 'constructor',
      configKeys: Object.keys(config),
      hasStateManager: !!config.stateManager,
    })

    this.config = { ...defaultInkConfig, ...config }
    this.state = {
      mode: 'interactive',
      isProcessing: false,
      isStreamingActive: false,
      currentInput: '',
      history: [],
      startTime: new Date(),
      lastActivity: new Date(),
    }

    // Store stateManager from config
    this.stateManager = config.stateManager
  }

  // Implementation of new ICLIFramework method
  subscribeToStateChanges(stateManager: any): void {
    this.stateManager = stateManager
    if (this.stateUnsubscribe) {
      this.stateUnsubscribe()
    }
    if (stateManager?.subscribe) {
      this.stateUnsubscribe = stateManager.subscribe((change: any) => {
        this.logger.debug('🔄 StateChange received in InkCLI', undefined, {
          component: 'InkCLIFramework',
          method: 'subscribeToStateChanges',
          changeType: change.type,
          changeField: change.field,
        })
        // Trigger UI updates for state changes that affect display
        if (
          change.type === 'config' &&
          (change.field === 'promptModel' || change.field === 'promptProvider')
        ) {
          // Force UI re-render for model/provider changes
          this.forceUIUpdate()
        }
      })
    }
  }

  // Force UI update by re-rendering the React component
  private forceUIUpdate(): void {
    // The React component will automatically re-render when state changes
    // This method is here for any explicit UI refresh needs
    this.logger.debug('🔄 Forcing UI update', undefined, {
      component: 'InkCLIFramework',
      method: 'forceUIUpdate',
    })
  }

  // ==============================================
  // ICLIFramework Implementation
  // ==============================================

  async initialize(): Promise<void> {
    if (this.isInitialized) return

    // Verify Ink is available (deferred to runtime for bundling compatibility)
    const isDevelopment = process.env.NODE_ENV !== 'production'
    if (!isDevelopment) {
      console.warn('⚠️ Ink framework availability not verified in production mode')
    }

    // Initialize HotkeyManager if hotkeys are enabled and message queue is available
    if (this.config.enableHotkeys && this.messageQueue) {
      this.hotkeyManager = new HotkeyManager(this.messageQueue, {
        enableShiftTab: true,
        enableEscape: true,
        enableCtrlC: true,
        passthrough: true,
      })

      this.logger.debug('InkCLIFramework HotkeyManager initialized', undefined, {
        component: 'InkCLIFramework',
        enableHotkeys: this.config.enableHotkeys,
      })
    }

    this.isInitialized = true
  }

  async start(): Promise<void> {
    if (!this.isInitialized) {
      await this.initialize()
    }

    if (this.renderInstance) {
      const error = inkFrameworkError('CLI_ALREADY_STARTED', 'CLI framework is already started', {
        framework: 'ink',
        operation: 'start',
        renderInstance: true,
      })
      this.logger.error('Cannot start CLI - already started', error)
      return Promise.reject(error)
    }

    // Enable hotkeys after start - only in interactive TTY environments
    if (this.hotkeyManager && process.stdin.isTTY) {
      this.hotkeyManager.enable()
      this.logger.debug('InkCLIFramework HotkeyManager enabled', undefined, {
        component: 'InkCLIFramework',
        method: 'start',
      })
    } else if (this.hotkeyManager) {
      this.logger.debug(
        'InkCLIFramework HotkeyManager not enabled (non-interactive environment)',
        undefined,
        {
          component: 'InkCLIFramework',
          method: 'start',
        }
      )
    }

    // Render the Ink application with custom Ctrl+C handling
    this.renderInstance = render(
      <InkCLIApp framework={this} config={this.config} initialState={this.state} />,
      { exitOnCtrlC: false }
    )
  }

  async shutdown(): Promise<void> {
    if (this.isShutdown) return

    this.isShutdown = true

    if (this.renderInstance) {
      this.renderInstance.unmount()
      this.renderInstance = null
    }

    // Clean up HotkeyManager
    if (this.hotkeyManager) {
      this.hotkeyManager.destroy()
      this.logger.debug('InkCLIFramework HotkeyManager destroyed', undefined, {
        component: 'InkCLIFramework',
        method: 'shutdown',
      })
      this.hotkeyManager = undefined
    }

    // Clean up StateManager subscription
    if (this.stateUnsubscribe) {
      this.stateUnsubscribe()
      this.stateUnsubscribe = null
    }
  }

  getState(): CLIState {
    return { ...this.state }
  }

  setMode(mode: CLIMode): void {
    const previousMode = this.state.mode
    // QiCore debug logging
    this.logger.debug(`MODE CHANGE: ${previousMode} → ${mode}`)
    this.logger.debug('🎨 InkCLIFramework: setMode called', undefined, {
      component: 'InkCLIFramework',
      method: 'setMode',
      previousMode,
      newMode: mode,
    })

    this.state.mode = mode
    this.state.lastActivity = new Date()

    // Mode changes are now handled through StateManager if needed
    this.logger.debug('🔄 Mode changed', undefined, {
      component: 'InkCLIFramework',
      method: 'setMode',
      previousMode,
      newMode: mode,
    })

    this.logger.debug('🎨 InkCLIFramework: setMode completed - state updated', undefined, {
      component: 'InkCLIFramework',
      method: 'setMode',
      mode,
    })
  }

  getMode(): CLIMode {
    return this.state.mode
  }

  showPrompt(): void {
    // In Ink, this is handled by the React component
  }

  handleInput(input: string): void {
    this.debug.log(`handleInput: "${input}"`)
    this.state.currentInput = input
    this.state.history.push(input)
    if (this.state.history.length > this.config.historySize) {
      this.state.history = this.state.history.slice(-this.config.historySize)
    }
    this.state.lastActivity = new Date()

    // CRITICAL: Set processing state when handling input
    this.state.isProcessing = true
    this.debug.log(`Set isProcessing to true for input processing`)

    // Handle exit commands: clear input and enqueue exit message
    if (input.trim() === '/exit' || input.trim() === '/quit') {
      // Clear input box in UI before exiting
      this.state.currentInput = ''
      this.logger.debug('🔄 Clearing input for exit command', undefined, {
        component: 'InkCLIFramework',
        method: 'handleInput',
        command: 'exit',
      })

      if (this.messageQueue) {
        this.messageQueue.enqueue({
          type: MessageType.USER_INPUT,
          input: input,
          raw: false,
          source: 'cli' as const,
          timestamp: new Date(),
          id: generateUniqueId(),
          priority: 2 as const,
        })
      }
      return
    }

    // v-0.6.1: InkCLIFramework handles message enqueuing for React-based input
    // (EventDrivenCLI inputManager is not used in hybrid/ink mode)
    if (this.messageQueue) {
      this.debug.log(`Enqueuing to message queue: "${input}"`)
      this.messageQueue.enqueue({
        type: MessageType.USER_INPUT,
        input: input,
        raw: false,
        source: 'cli' as const,
        timestamp: new Date(),
        id: generateUniqueId(),
        priority: 2 as any,
      })
    } else {
      this.logger.error('FATAL: No message queue - input lost!', undefined, {
        component: 'InkCLIFramework',
        method: 'handleInput',
        input: input,
        severity: 'FATAL',
        errorContext: 'no_message_queue',
      })
    }
  }

  displayMessage(content: string, type: ICLIFrameworkMessageType = 'info'): void {
    // FIX: Prevent displaying empty or whitespace-only messages that cause empty lines
    if (!content || content.trim() === '') {
      this.debug.warn(`Skipping empty message display, type: ${type}`)
      return
    }

    this.debug.log(`displayMessage: ${content}`)
    this.debug.log(`displayMessage type: ${type}`)
    this.debug.log(`displayMessage content length: ${content.length}`)

    // CRITICAL: Reset processing state when displaying final message
    this.state.isProcessing = false
    this.debug.log(`Reset isProcessing to false`)

    // CRITICAL: Add message to React component UI (pure message-driven)
    if (this.setMessages) {
      const assistantMessage = createAssistantMessage(content)
      this.setMessages((prev) => [...prev, assistantMessage])
      this.debug.log(`Added message to React UI: "${content}"`)
    } else {
      this.debug.warn(`No setMessages callback registered - message not displayed in UI`)
    }

    // Essential: Ready for new input state managed directly
    this.debug.log(`Ready for new input after displaying message`)
  }

  /**
   * CRITICAL: Reset processing state to stop infinite loading
   * Called by QiPromptCLI after processing messages
   */
  resetProcessingState(): void {
    this.state.isProcessing = false
    this.debug.log(`🔄 Explicitly reset processing state`)
  }

  /**
   * Register React component's setMessages callback for pure message-driven updates
   */
  registerMessageCallback(
    setMessages: (updater: (prev: OutputMessage[]) => OutputMessage[]) => void
  ): void {
    this.setMessages = setMessages
  }

  displayProgress(phase: string, progress: number, details?: string): void {
    // FIX: Prevent empty or meaningless progress events that cause empty line rendering
    if (!phase || phase.trim() === '' || progress < 0 || progress > 100) {
      this.debug.warn(`Skipping invalid progress: phase="${phase}", progress=${progress}`)
      return
    }

    this.debug.log(`Progress: ${phase} ${progress}% - Details: ${details}`)
    this.debug.log(`🔍 PROGRESS MESSAGE - This might be the workflow messages you're seeing!`)

    // Progress updates now handled through direct state management
  }

  startStreaming(): void {
    this.state.isStreamingActive = true

    // Streaming state managed directly
  }

  addStreamingChunk(content: string): void {
    if (this.state.isStreamingActive) {
      // Streaming chunks handled through direct state management
    }
  }

  completeStreaming(message?: string): void {
    this.state.isStreamingActive = false
    const totalTime = Date.now() - this.state.startTime.getTime()

    // Streaming completion handled through direct state management

    if (message) {
      this.displayMessage(message, 'complete')
    }
  }

  cancelStreaming(): void {
    this.state.isStreamingActive = false

    // Streaming cancellation handled through direct state management
  }

  updateConfig(config: Partial<CLIConfig>): void {
    this.config = { ...this.config, ...config }
  }

  getConfig(): CLIConfig {
    return { ...this.config }
  }

  // ==============================================
  // IAgentCLIBridge Implementation
  // ==============================================

  connectAgent(agent: any): void {
    this.connectedAgent = agent

    // v-0.6.1: Agent communication handled through message queue only
    // No EventEmitter listeners - prevents dual architecture
  }

  disconnectAgent(): void {
    // v-0.6.1: No EventEmitter cleanup needed - message queue handles communication
    this.connectedAgent = null
  }

  onAgentProgress(progress: { phase: string; progress: number; details?: string }): void {
    // FIX: Validate progress data before processing to prevent empty line rendering
    if (!progress || !progress.phase || progress.phase.trim() === '') {
      this.debug.warn(`Ignoring invalid progress event:`, progress)
      return
    }

    this.state.isProcessing = true
    this.displayProgress(progress.phase, progress.progress, progress.details)
  }

  onAgentMessage(message: { content: string; type: string }): void {
    // FIX: Validate message content to prevent empty line rendering
    if (!message || !message.content || message.content.trim() === '') {
      this.debug.warn(`Ignoring empty agent message:`, message)
      return
    }

    this.displayMessage(message.content, message.type as ICLIFrameworkMessageType)
  }

  onAgentComplete(result: any): void {
    this.state.isProcessing = false

    // Extract the actual response content
    let responseContent = 'Task completed successfully'
    if (result) {
      if (typeof result === 'string') {
        responseContent = result
      } else if (result.content) {
        responseContent = result.content
      } else if (result.data) {
        responseContent = result.data
      } else if (result.response) {
        responseContent = result.response
      }
    }

    // Emit the completion with the actual response
    this.displayMessage(responseContent, 'complete')

    // Progress state cleared through direct state management
  }

  onAgentError(error: any): void {
    this.state.isProcessing = false
    const errorMessage = error?.message || 'An error occurred'
    this.displayMessage(`Error: ${errorMessage}`, 'error')

    // Error handling through direct state management
    this.logger.error('Agent error occurred', undefined, {
      component: 'InkCLIFramework',
      method: 'onAgentError',
      errorMessage: error?.message || 'Unknown error',
      errorStack: error?.stack,
      errorContext: 'agent_error_handling',
    })
  }

  onAgentCancelled(reason: string): void {
    // Prevent recursive cancellation calls that cause infinite loops
    if (this.isCancelling) {
      return
    }

    this.isCancelling = true
    this.state.isProcessing = false
    this.displayMessage(`Task cancelled: ${reason}`, 'warning')

    // Cancellation handling through direct state management

    // Reset the cancellation flag after a brief delay
    setTimeout(() => {
      this.isCancelling = false
    }, 100)
  }

  sendToAgent(input: string): void {
    if (this.connectedAgent && typeof this.connectedAgent.process === 'function') {
      this.state.isProcessing = true
      this.connectedAgent.process({ input, context: {} })
    }
  }

  cancelAgent(): void {
    // Prevent recursive cancellation calls
    if (this.isCancelling) {
      return
    }

    if (this.connectedAgent && typeof this.connectedAgent.cancel === 'function') {
      this.isCancelling = true
      this.connectedAgent.cancel()

      // Reset cancellation flag after a brief delay
      setTimeout(() => {
        this.isCancelling = false
      }, 100)
    }
  }
}

// ==============================================
// React Component
// ==============================================

interface InkCLIAppProps {
  framework: InkCLIFramework
  config: CLIConfig
  initialState: CLIState
}

function InkCLIApp({ framework, config, initialState }: InkCLIAppProps) {
  // Initialize messages with welcome message
  const [messages, setMessages] = useState<OutputMessage[]>(() => [
    createSystemMessage('Welcome to Qi CLI with Ink! Type /help for commands.'),
  ])
  const [state, setState] = useState({
    ...initialState,
    currentPhase: '',
  })
  const [taskName] = useState<string>()
  const [providerInfo, setProviderInfo] = useState({ provider: 'ollama', model: 'qwen3:0.6b' })
  const [permissionRequest, setPermissionRequest] = useState<PermissionRequest | null>(null)
  const [isCancelling, setIsCancelling] = useState(false)
  const { exit } = useApp()

  // Get real provider/model info from StateManager
  useEffect(() => {
    const getAgentInfo = () => {
      const safeGetAgentInfo = fromTryCatch(
        () => {
          // Type-safe access to framework internals
          const typedFramework = framework as unknown as FrameworkWithInternals
          const stateManager = typedFramework.config?.stateManager || typedFramework.stateManager
          const debugMode = typedFramework.debugMode || false

          if (debugMode) {
            console.log('🔍 [DEBUG] getAgentInfo - stateManager found:', !!stateManager)
          }

          if (stateManager?.getPromptConfig) {
            const promptConfig = stateManager.getPromptConfig()
            if (debugMode) {
              console.log('🔍 [DEBUG] getAgentInfo - promptConfig:', promptConfig)
            }
            if (promptConfig?.provider && promptConfig?.model) {
              if (debugMode) {
                console.log(
                  '🔍 [DEBUG] getAgentInfo - updating UI:',
                  promptConfig.provider,
                  promptConfig.model
                )
              }
              setProviderInfo({
                provider: promptConfig.provider,
                model: promptConfig.model,
              })
              return
            }
          }

          // Fallback: Try to get from connected agent if StateManager approach fails
          const agent = typedFramework.connectedAgent
          if (agent) {
            // Fallback to getting from prompt handler
            if (agent.promptHandler?.getCurrentModel) {
              const currentModel = agent.promptHandler.getCurrentModel()
              if (currentModel) {
                setProviderInfo((prev) => ({ ...prev, model: currentModel }))
              }
            }
          }
        },
        (error) => create('AGENT_INFO_FAILED', `Failed to get agent info: ${error}`, 'SYSTEM')
      )

      // Result is ignored as this is optional UI enhancement
      // Keep default values if we can't get agent info
    }

    // Update agent info initially and when framework changes
    getAgentInfo()

    // Listen for state changes to refresh immediately
    const handleStateChange = (event: any) => {
      const typedFramework = framework as unknown as FrameworkWithInternals
      const debugMode = typedFramework.debugMode || false
      if (debugMode) {
        console.log('🔍 [DEBUG] StateChange received:', event.type, event.field)
      }
      // Refresh provider info when config changes (provider/model switches)
      if (event.type === 'config') {
        if (debugMode) {
          console.log('🔍 [DEBUG] Config change detected, refreshing agent info')
        }
        getAgentInfo()
      }
    }

    // Subscribe to state changes from the state manager
    let unsubscribe: (() => void) | null = null
    const setupSubscription = fromTryCatch(
      () => {
        const typedFramework = framework as unknown as FrameworkWithInternals
        const stateManager = typedFramework.config?.stateManager || typedFramework.stateManager
        const debugMode = typedFramework.debugMode || false

        if (debugMode) {
          console.log('🔍 [DEBUG] Subscription setup - stateManager found:', !!stateManager)
        }
        if (stateManager?.subscribe) {
          if (debugMode) {
            console.log('🔍 [DEBUG] Setting up StateManager subscription')
          }
          unsubscribe = stateManager.subscribe(handleStateChange)
          if (debugMode) {
            console.log('🔍 [DEBUG] StateManager subscription active')
          }
        } else {
          if (debugMode) {
            console.log('🔍 [DEBUG] No stateManager.subscribe method found')
          }
        }
      },
      (error) =>
        create('SUBSCRIPTION_SETUP_FAILED', `Failed to setup subscription: ${error}`, 'SYSTEM')
    )
    // Result is ignored as subscription is optional UI enhancement

    // Set up an interval to refresh agent info periodically as fallback
    const interval = setInterval(getAgentInfo, 2000)

    return () => {
      clearInterval(interval)
      // Clean up state change listener
      if (unsubscribe) {
        const cleanupResult = fromTryCatch(
          () => unsubscribe!(),
          (error) => create('CLEANUP_FAILED', `Failed to cleanup subscription: ${error}`, 'SYSTEM')
        )
        // Ignore cleanup result as this is just cleanup
      }
    }
  }, [framework])

  // Register setMessages callback with framework for pure message-driven updates
  useEffect(() => {
    framework.registerMessageCallback(setMessages)
    return () => {
      // Clean up callback on unmount
      framework.registerMessageCallback((() => {}) as any)
    }
  }, [framework, setMessages])

  // Sync React state with framework state periodically (pure message-driven approach)
  useEffect(() => {
    const syncState = () => {
      const frameworkState = framework.getState()
      setState((prev) => ({
        ...prev,
        isProcessing: frameworkState.isProcessing,
        mode: frameworkState.mode,
        currentPhase: frameworkState.isProcessing ? 'Processing' : '',
      }))
    }

    // Initial sync
    syncState()

    // Sync every 100ms to keep React UI in sync with framework state
    const interval = setInterval(syncState, 100)

    return () => clearInterval(interval)
  }, [framework])

  // Event subscriptions removed - UI updates now handled through StateManager subscriptions above

  const handleInput = useCallback(
    (input: string) => {
      // Add user's input to the message display using Claude Code style
      const userMessage = createUserMessage(input)
      setMessages((prev) => [...prev, userMessage])

      // FIX: Don't manage processing state in React - let framework handle it
      // The framework will update its state and we'll sync from it

      // Pure message-driven: only call framework.handleInput
      framework.handleInput(input)
    },
    [framework]
  )

  const handleStateChange = useCallback(() => {
    const currentMode = framework.getMode()
    const modes: CLIMode[] = ['interactive', 'command', 'streaming']
    const currentIndex = modes.indexOf(currentMode)
    const nextIndex = (currentIndex + 1) % modes.length
    const nextMode = modes[nextIndex]
    if (nextMode) {
      framework.setMode(nextMode)
    }
  }, [framework])

  const handleCommand = useCallback(
    (command: string, args: string[]) => {
      // Add command as user message (like user typed it)
      const commandMessage = createUserMessage(`/${command} ${args.join(' ')}`)
      setMessages((prev) => [...prev, commandMessage])

      // Handle built-in commands locally
      switch (command) {
        case 'help':
          const helpMessage = createSystemMessage(
            'Available commands:\n' +
              '/help - Show this help message\n' +
              '/clear - Clear conversation history\n' +
              '/model - Switch AI model (not implemented)\n' +
              '/status - Show system status\n' +
              '/config - View configuration\n' +
              '/permission - Demo permission dialog'
          )
          setMessages((prev) => [...prev, helpMessage])
          break

        case 'clear':
          setMessages([createSystemMessage('Conversation history cleared')])
          break

        case 'status':
          const statusMessage = createSystemMessage(
            `System Status:\n` +
              `Mode: ${state.mode}\n` +
              `Processing: ${state.isProcessing ? 'Yes' : 'No'}\n` +
              `Provider: ${providerInfo.provider}\n` +
              `Model: ${providerInfo.model}\n` +
              `Uptime: ${Math.floor((Date.now() - state.startTime.getTime()) / 1000)}s`
          )
          setMessages((prev) => [...prev, statusMessage])
          break

        case 'config':
          const configMessage = createSystemMessage(
            `Configuration:\n` +
              `Colors: ${config.colors ? 'Enabled' : 'Disabled'}\n` +
              `Animations: ${config.animations ? 'Enabled' : 'Disabled'}\n` +
              `History Size: ${config.historySize}\n` +
              `Streaming: ${config.enableStreaming ? 'Enabled' : 'Disabled'}`
          )
          setMessages((prev) => [...prev, configMessage])
          break

        case 'permission':
          const request = createPermissionRequest(
            'file-editor',
            'modify files',
            'The AI wants to edit source code files in your project.',
            [
              'May overwrite existing files',
              'Could introduce bugs or security issues',
              'Changes might be difficult to revert',
            ]
          )
          setPermissionRequest(request)
          break

        default:
          // Unrecognized commands handled by message queue
          const commandInput = `/${command} ${args.join(' ')}`
          framework.handleInput(commandInput)
      }
    },
    [framework, state, config, providerInfo]
  )

  const handleCancel = useCallback(() => {
    // Prevent recursive cancellation calls
    if (isCancelling) return

    // Only proceed if actually processing
    if (!state.isProcessing) return

    setIsCancelling(true)

    // Add a brief cancelled message for user feedback
    const cancelMessage = createSystemMessage('🛑 Operation cancelled')
    setMessages((prev) => [...prev, cancelMessage])

    // Reset processing state
    setState((prev) => ({
      ...prev,
      isProcessing: false,
      currentPhase: '',
      isStreamingActive: false,
    }))

    // Direct cancellation through agent bridge
    framework.cancelAgent()

    // Reset cancellation flag after a brief delay
    setTimeout(() => {
      setIsCancelling(false)
    }, 100)
  }, [framework, state.isProcessing, isCancelling])

  const handleClear = useCallback(() => {
    // Clear input without adding system feedback message
    // The visual feedback is the cleared input itself
  }, [])

  const handleEscapeWhenIdle = useCallback(() => {
    // Since we're in a React component scope, we can only emit events
    // The actual input clearing will be handled by the InputBox component
    const infoMessage = createSystemMessage('⏹️ ESC - No active operations to cancel')
    setMessages((prev) => [...prev, infoMessage])

    // Direct UI feedback only
  }, [framework])

  // Permission dialog handlers
  const handlePermissionApprove = useCallback((_requestId: string, remember?: boolean) => {
    const systemMessage = createSystemMessage(
      `Tool permission approved${remember ? ' (remembered)' : ''}`
    )
    setMessages((prev) => [...prev, systemMessage])
    setPermissionRequest(null)
    // Permission handling through message queue if needed
  }, [])

  const handlePermissionDeny = useCallback((_requestId: string, remember?: boolean) => {
    const systemMessage = createSystemMessage(
      `Tool permission denied${remember ? ' (remembered)' : ''}`
    )
    setMessages((prev) => [...prev, systemMessage])
    setPermissionRequest(null)
    // Permission handling through message queue if needed
  }, [])

  const handlePermissionDismiss = useCallback(() => {
    setPermissionRequest(null)
  }, [])

  // Create logger for use in useInput callback
  const logger = createDebugLogger('InkCLIApp')

  // Global input handler to intercept Ctrl+C, Esc, and Shift+Tab before TextInput can block them
  useInput((inputChar, key) => {
    // Handle Ctrl+C to clear input - high priority handler
    if (key.ctrl && inputChar === 'c') {
      // Direct input clearing handled by UI components
      return
    }

    // Handle Ctrl+D for exit
    if (key.ctrl && inputChar === 'd') {
      exit()
      return
    }

    // Handle Esc to cancel processing
    if (key.escape) {
      // Always handle ESC key for better user experience
      if (state.isProcessing) {
        // Call the cancel handler that's passed to MainLayout
        handleCancel()
      } else {
        // If not processing, clear input and show feedback
        handleEscapeWhenIdle()
      }
      return
    }

    // Handle Shift+Tab for mode cycling (fallback if HotkeyManager doesn't work)
    if (key.shift && key.tab) {
      logger.trace('Ink useInput: Detected Shift+Tab - cycling mode')
      handleStateChange()
      return
    }
  })

  return (
    <MainLayout
      state={state.isProcessing ? 'busy' : 'ready'}
      subState={
        state.mode === 'interactive' ? 'generic' : state.mode === 'command' ? 'planning' : 'editing'
      }
      taskName={taskName}
      messages={messages}
      onInput={handleInput}
      onStateChange={handleStateChange}
      onCommand={handleCommand}
      onCancel={handleCancel}
      onClear={handleClear}
      provider={providerInfo.provider}
      model={providerInfo.model}
      mode={state.mode}
      isProcessing={state.isProcessing}
      currentPhase={state.currentPhase}
      framework={framework}
      permissionRequest={permissionRequest}
      onPermissionApprove={handlePermissionApprove}
      onPermissionDeny={handlePermissionDeny}
      onPermissionDismiss={handlePermissionDismiss}
    />
  )
}

/**
 * Factory function to create Ink CLI
 */
export function createInkCLIImpl(config: Partial<CLIConfig> = {}): ICLIFramework {
  return new InkCLIFramework(config)
}
