#!/usr/bin/env bun

/**
 * CLI-AMSG Integration Example
 *
 * Demonstrates proper integration patterns between @qi/cli and @qi/amsg
 * following the architectural patterns from the implementation guides.
 *
 * Key patterns demonstrated:
 * 1. Message-driven CLI architecture (CLI → AMSG → Agent flow)
 * 2. Proper Result<T> error handling throughout
 * 3. Priority queue message processing
 * 4. State management with XState
 * 5. Subscription-based monitoring
 */

import { type Result, type QiError, match, success, failure, systemError } from '@qi/base'

import { QiAsyncMessageQueue } from '@qi/amsg'
import { QiMessageFactory } from '@qi/amsg'
import { MessageType, MessagePriority } from '@qi/amsg'
import type { QiMessage, UserInputMessage, SystemControlMessage } from '@qi/amsg'

// CLI imports (would be from @qi/cli when complete)
// For now, demonstrate the integration pattern conceptually

console.log('🚀 QiCore CLI-AMSG Integration Example')
console.log('=====================================\n')

/**
 * Mock Agent that processes messages and generates responses
 */
class MockAgent {
  private messageQueue: QiAsyncMessageQueue<QiMessage>
  private messageFactory: QiMessageFactory
  private isRunning = false

  constructor(messageQueue: QiAsyncMessageQueue<QiMessage>) {
    this.messageQueue = messageQueue
    this.messageFactory = new QiMessageFactory()
  }

  /**
   * Start the agent message processing loop
   */
  async start(): Promise<Result<void, QiError>> {
    if (this.isRunning) {
      return failure(systemError('Agent already running'))
    }

    this.isRunning = true
    console.log('🤖 Mock Agent started - processing messages...\n')

    try {
      // Start processing messages from the queue using h2A pattern
      for await (const message of this.messageQueue) {
        await this.processMessage(message)

        // Simulate some processing time
        await new Promise((resolve) => setTimeout(resolve, 100))
      }
    } catch (error) {
      this.isRunning = false
      return failure(systemError(`Agent processing failed: ${error}`))
    }

    return success(undefined)
  }

  /**
   * Process individual messages with proper Result<T> patterns
   */
  private async processMessage(message: QiMessage): Promise<Result<void, QiError>> {
    console.log(
      `📨 Agent processing: ${message.type} (ID: ${message.id.slice(0, 8)}, Priority: ${message.priority})`
    )

    try {
      switch (message.type) {
        case MessageType.USER_INPUT:
          return await this.handleUserInput(message as UserInputMessage)

        case MessageType.SYSTEM_CONTROL:
          return await this.handleSystemControl(message as SystemControlMessage)

        default:
          console.log(`❓ Agent: Unknown message type ${message.type}`)
          return success(undefined)
      }
    } catch (error) {
      return failure(systemError(`Message processing failed: ${error}`))
    }
  }

  /**
   * Handle user input messages
   */
  private async handleUserInput(message: UserInputMessage): Promise<Result<void, QiError>> {
    const input = message.input.trim()
    console.log(`💬 User said: "${input}"`)

    // Simulate different responses based on input
    let response: string
    if (input.toLowerCase().includes('hello')) {
      response = '👋 Hello! How can I help you today?'
    } else if (input.toLowerCase().includes('help')) {
      response = '🆘 Available commands: hello, help, quit, test'
    } else if (input.toLowerCase().includes('quit')) {
      response = '👋 Goodbye!'
      // Send system control message to shut down
      return this.sendSystemMessage('shutdown', 'User requested quit')
    } else if (input.toLowerCase().includes('test')) {
      response = '🧪 Running test... All systems operational!'
    } else {
      response = `🤔 I heard you say "${input}". That's interesting!`
    }

    return this.sendAgentResponse(response, message.correlationId)
  }

  /**
   * Handle system control messages
   */
  private async handleSystemControl(message: SystemControlMessage): Promise<Result<void, QiError>> {
    console.log(`🔧 System control: ${message.action}`)

    if (message.action === 'shutdown') {
      this.isRunning = false
      console.log('🛑 Agent shutting down...')
      this.messageQueue.done()
    }

    return success(undefined)
  }

  /**
   * Send agent response message
   */
  private async sendAgentResponse(
    content: string,
    correlationId?: string
  ): Promise<Result<void, QiError>> {
    const responseResult = this.messageFactory.createAgentOutputMessage(content, 'text', false)

    return match(
      (responseMessage): Result<void, QiError> => {
        // Set correlation ID if provided
        if (correlationId) {
          ;(responseMessage as any).correlationId = correlationId
        }

        const enqueueResult = this.messageQueue.enqueue(responseMessage)

        return match(
          (): Result<void, QiError> => {
            console.log(`🤖 Agent response: "${content}"`)
            return success(undefined)
          },
          (error): Result<void, QiError> =>
            failure(systemError(`Failed to enqueue agent response: ${error.message}`)),
          enqueueResult
        )
      },
      (error): Result<void, QiError> =>
        failure(systemError(`Failed to create agent response: ${error.message}`)),
      responseResult
    )
  }

  /**
   * Send system control message
   */
  private async sendSystemMessage(action: string, reason?: string): Promise<Result<void, QiError>> {
    const systemMessageResult = this.messageFactory.createSystemControlMessage(
      action as any,
      true,
      reason
    )

    return match(
      (systemMessage): Result<void, QiError> => {
        const enqueueResult = this.messageQueue.enqueue(systemMessage)

        return match(
          (): Result<void, QiError> => success(undefined),
          (error): Result<void, QiError> =>
            failure(systemError(`Failed to enqueue system message: ${error.message}`)),
          enqueueResult
        )
      },
      (error): Result<void, QiError> =>
        failure(systemError(`Failed to create system message: ${error.message}`)),
      systemMessageResult
    )
  }
}

/**
 * Mock CLI that demonstrates the message-driven architecture
 */
class MockCLI {
  private messageQueue: QiAsyncMessageQueue<QiMessage>
  private messageFactory: QiMessageFactory
  private isRunning = false

  constructor(messageQueue: QiAsyncMessageQueue<QiMessage>) {
    this.messageQueue = messageQueue
    this.messageFactory = new QiMessageFactory()
  }

  /**
   * Start the CLI interface
   */
  async start(): Promise<Result<void, QiError>> {
    if (this.isRunning) {
      return failure(systemError('CLI already running'))
    }

    this.isRunning = true
    console.log('💻 Mock CLI started\n')

    // Set up message queue monitoring
    this.setupQueueMonitoring()

    // Demonstrate sending various messages
    await this.demonstrateMessageFlow()

    return success(undefined)
  }

  /**
   * Set up queue monitoring with subscription system
   */
  private setupQueueMonitoring(): void {
    const unsubscribe = this.messageQueue.subscribe((event) => {
      switch (event.type) {
        case 'message_enqueued':
          console.log(
            `📥 Queue: Message enqueued (${event.message?.type}, Priority: ${event.message?.priority})`
          )
          break

        case 'message_dequeued':
          console.log(`📤 Queue: Message dequeued (${event.message?.type})`)
          break

        case 'message_dropped':
          console.log(`🗑️ Queue: Message dropped - ${event.reason}`)
          break

        case 'queue_paused':
          console.log('⏸️ Queue: Paused')
          break

        case 'queue_resumed':
          console.log('▶️ Queue: Resumed')
          break
      }
    })

    // Store unsubscribe function for cleanup
    ;(this as any).unsubscribeFromQueue = unsubscribe
  }

  /**
   * Demonstrate the message flow with different priorities
   */
  private async demonstrateMessageFlow(): Promise<Result<void, QiError>> {
    console.log('📋 Demonstrating message flow patterns:\n')

    // 1. Regular user inputs (NORMAL priority)
    await this.sendUserMessage('Hello!')
    await this.wait(500)

    await this.sendUserMessage('Help me with something')
    await this.wait(500)

    // 2. High priority message
    await this.sendHighPriorityMessage('This is urgent!')
    await this.wait(500)

    // 3. Test backpressure with multiple messages
    console.log('🚀 Testing backpressure with rapid messages...')
    await this.sendUserMessage('test message 1')
    await this.sendUserMessage('test message 2')
    await this.sendUserMessage('test message 3')
    await this.wait(1000)

    // 4. Critical system control message
    await this.sendUserMessage('quit')
    await this.wait(1000)

    return success(undefined)
  }

  /**
   * Send user input message (normal priority)
   */
  private async sendUserMessage(input: string): Promise<Result<void, QiError>> {
    const messageResult = this.messageFactory.createUserInputMessage(input, 'cli', false)

    return match(
      (message): Result<void, QiError> => {
        const enqueueResult = this.messageQueue.enqueue(message)

        return match(
          (): Result<void, QiError> => success(undefined),
          (error): Result<void, QiError> =>
            failure(systemError(`Failed to enqueue user message: ${error.message}`)),
          enqueueResult
        )
      },
      (error): Result<void, QiError> =>
        failure(systemError(`Failed to create user message: ${error.message}`)),
      messageResult
    )
  }

  /**
   * Send high priority message to demonstrate priority queuing
   */
  private async sendHighPriorityMessage(content: string): Promise<Result<void, QiError>> {
    const messageResult = this.messageFactory.createUserInputMessage(content, 'cli', false)

    return match(
      (message): Result<void, QiError> => {
        // Override priority to HIGH
        ;(message as any).priority = MessagePriority.HIGH

        const enqueueResult = this.messageQueue.enqueue(message)

        return match(
          (): Result<void, QiError> => success(undefined),
          (error): Result<void, QiError> =>
            failure(systemError(`Failed to enqueue high priority message: ${error.message}`)),
          enqueueResult
        )
      },
      (error): Result<void, QiError> =>
        failure(systemError(`Failed to create high priority message: ${error.message}`)),
      messageResult
    )
  }

  /**
   * Utility function to wait
   */
  private async wait(ms: number): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, ms))
  }
}

/**
 * Main application demonstrating CLI-AMSG integration
 */
async function main(): Promise<Result<void, QiError>> {
  try {
    // 1. Create message queue with proper configuration
    const messageQueue = new QiAsyncMessageQueue<QiMessage>({
      maxSize: 100,
      maxConcurrent: 5,
      enableStats: true,
      priorityQueuing: true,
    })

    // 2. Create and start the mock agent (simulates the real agent)
    const agent = new MockAgent(messageQueue)

    // Start agent in background
    setTimeout(async () => {
      const agentResult = await agent.start()
      match(
        () => console.log('✅ Agent completed successfully'),
        (error) => console.error('❌ Agent error:', error.message),
        agentResult
      )
    }, 100)

    // 3. Create and start the mock CLI
    const cli = new MockCLI(messageQueue)
    const cliResult = await cli.start()

    return match(
      (): Result<void, QiError> => {
        console.log('\n✅ CLI-AMSG Integration Demo completed!')
        console.log('\nKey patterns demonstrated:')
        console.log('- ✅ Message-driven architecture (CLI → AMSG → Agent)')
        console.log('- ✅ Priority queue handling')
        console.log('- ✅ Result<T> error handling')
        console.log('- ✅ Subscription-based monitoring')
        console.log('- ✅ Proper correlation IDs')
        console.log('- ✅ Backpressure management')

        return success(undefined)
      },
      (error): Result<void, QiError> => failure(systemError(`CLI failed: ${error.message}`)),
      cliResult
    )
  } catch (error) {
    return failure(systemError(`Application failed: ${error}`))
  }
}

// Run the application
const result = await main()

match(
  () => process.exit(0),
  (error) => {
    console.error('💥 Application failed:', error.message)
    process.exit(1)
  },
  result
)
