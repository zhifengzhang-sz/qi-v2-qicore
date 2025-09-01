/**
 * Readline Input Manager - v-0.6.1 Pure Message-Driven
 *
 * COMPLETELY REDESIGNED: No EventEmitter, all input goes through message queue
 * Handles terminal input and sends USER_INPUT messages to queue
 */

import { createInterface, type Interface } from 'node:readline'
import type { QiAsyncMessageQueue } from '@qi/amsg'
import type { QiMessage } from '@qi/amsg'
import { MessagePriority, MessageType } from '@qi/amsg'
import { createDebugLogger } from '../../utils/DebugLogger'

export interface ReadlineConfig {
  prompt: string
  historySize: number
  enableHistory: boolean
}

/**
 * Manages readline input and sends messages to queue
 * v-0.6.1: Pure message-driven - NO event listeners
 */
export class ReadlineInputManager {
  private rl: Interface | null = null
  private config: ReadlineConfig
  private history: string[] = []
  private messageQueue: QiAsyncMessageQueue<QiMessage>
  private debug = createDebugLogger('ReadlineInputManager')
  private inputCallbacks: ((input: string) => void)[] = []

  constructor(messageQueue: QiAsyncMessageQueue<QiMessage>, config: Partial<ReadlineConfig> = {}) {
    this.messageQueue = messageQueue
    this.config = {
      prompt: '> ',
      historySize: 100,
      enableHistory: true,
      ...config,
    }
  }

  /**
   * Initialize readline interface
   */
  initialize(): void {
    if (this.rl) {
      this.debug.warn('ReadlineInputManager already initialized')
      return
    }

    this.rl = createInterface({
      input: process.stdin,
      output: process.stdout,
      prompt: this.config.prompt,
    })

    // v-0.6.1: Pure message-driven input handling
    this.setupMessageDrivenInput()

    this.debug.log('ReadlineInputManager initialized with message queue')
  }

  /**
   * Setup pure message-driven input handling
   */
  private setupMessageDrivenInput(): void {
    if (!this.rl) return

    // Handle line input through message queue
    this.rl.on('line', (input: string) => {
      this.handleInput(input.trim())
    })

    // Handle close gracefully
    this.rl.on('close', () => {
      this.sendSystemMessage('shutdown', { reason: 'user_exit' })
    })

    // Handle SIGINT (Ctrl+C)
    this.rl.on('SIGINT', () => {
      this.sendSystemMessage('shutdown', { reason: 'SIGINT' })
    })
  }

  /**
   * Handle input and send to message queue
   */
  private handleInput(input: string): void {
    // Add to history if enabled and not empty
    if (this.config.enableHistory && input) {
      this.addToHistory(input)
    }

    // Send USER_INPUT message to queue
    this.sendUserInputMessage(input)

    // Call registered input callbacks (for MessageDrivenCLI compatibility)
    this.inputCallbacks.forEach((callback) => {
      try {
        callback(input)
      } catch (error) {
        this.debug.warn('Error in input callback:', error)
      }
    })
  }

  /**
   * Send USER_INPUT message to queue
   */
  private sendUserInputMessage(input: string): void {
    const message: QiMessage = {
      id: Math.random().toString(36).substring(2, 15),
      type: MessageType.USER_INPUT,
      timestamp: new Date(),
      priority: MessagePriority.NORMAL,
      input,
      raw: false,
      source: 'cli' as const,
    }

    this.debug.log(`Sending USER_INPUT message: "${input}"`)
    this.messageQueue.enqueue(message)
  }

  /**
   * Send system control message to queue
   */
  private sendSystemMessage(action: 'pause' | 'resume' | 'reset' | 'shutdown', data: any): void {
    const message: QiMessage = {
      id: Math.random().toString(36).substring(2, 15),
      type: MessageType.SYSTEM_CONTROL,
      timestamp: new Date(),
      priority: MessagePriority.HIGH,
      action,
      reason: data?.reason,
      immediate: true,
    }

    this.debug.log(`Sending SYSTEM_CONTROL message: ${action}`)
    this.messageQueue.enqueue(message)
  }

  /**
   * Add input to history
   */
  private addToHistory(input: string): void {
    this.history.unshift(input)
    if (this.history.length > this.config.historySize) {
      this.history = this.history.slice(0, this.config.historySize)
    }
  }

  /**
   * Show prompt for next input
   */
  showPrompt(): void {
    if (this.rl) {
      this.rl.prompt()
    }
  }

  /**
   * Clear current line
   */
  clearLine(): void {
    if (this.rl) {
      this.rl.write(null, { ctrl: true, name: 'u' })
    }
  }

  /**
   * Write text to output
   */
  write(text: string): void {
    if (this.rl) {
      this.rl.write(text)
    }
  }

  /**
   * Get command history
   */
  getHistory(): string[] {
    return [...this.history]
  }

  /**
   * Update configuration
   */
  updateConfig(newConfig: Partial<ReadlineConfig>): void {
    this.config = { ...this.config, ...newConfig }
    if (this.rl) {
      this.rl.setPrompt(this.config.prompt)
    }
  }

  /**
   * Register input callback for MessageDrivenCLI compatibility
   */
  onInput(callback: (input: string) => void): void {
    this.inputCallbacks.push(callback)
    this.debug.log('Input callback registered for MessageDrivenCLI compatibility')
  }

  /**
   * Close readline interface
   */
  close(): void {
    if (this.rl) {
      this.rl.close()
      this.rl = null
      this.debug.log('ReadlineInputManager closed')
    }
  }
}
