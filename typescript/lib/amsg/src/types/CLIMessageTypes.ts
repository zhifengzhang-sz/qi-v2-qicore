/**
 * CLI Message Types for v-0.6.1 EventEmitter → QiAsyncMessageQueue Migration
 *
 * Defines CLI-specific message types that replace EventEmitter events
 * in the EventDrivenCLI coordination system.
 */

import type { QiError } from '@qi/base'
import { create } from '@qi/base'
import type {
  AgentErrorMessage,
  AgentOutputMessage,
  StreamDataMessage,
  StreamEndMessage,
  StreamErrorMessage,
  StreamStartMessage,
  SystemControlMessage,
  SystemStatusMessage,
  UserInputMessage,
  UserInterruptMessage,
} from './MessageTypes.js'
import { MessagePriority, MessageType } from './MessageTypes.js'

/**
 * CLI-specific message types
 */
export enum CLIMessageType {
  // CLI Lifecycle events
  CLI_READY = 'cli_ready',
  CLI_SHUTDOWN = 'cli_shutdown',

  // User interaction events
  CLI_USER_INPUT = 'cli_user_input',
  CLI_COMMAND_REQUESTED = 'cli_command_requested',

  // UI state changes
  CLI_MODE_CHANGED = 'cli_mode_changed',
  CLI_PROMPT_UPDATED = 'cli_prompt_updated',

  // Display events
  CLI_MESSAGE_RECEIVED = 'cli_message_received',
  CLI_PROGRESS_UPDATE = 'cli_progress_update',

  // Streaming events
  CLI_STREAMING_STARTED = 'cli_streaming_started',
  CLI_STREAMING_CHUNK = 'cli_streaming_chunk',
  CLI_STREAMING_COMPLETE = 'cli_streaming_complete',
  CLI_STREAMING_CANCELLED = 'cli_streaming_cancelled',

  // Control events
  CLI_CANCEL_REQUESTED = 'cli_cancel_requested',
  CLI_ERROR_OCCURRED = 'cli_error_occurred',
}

/**
 * CLI lifecycle messages - extend existing QiMessage types
 */
export interface CLIReadyMessage extends SystemStatusMessage {
  subtype: CLIMessageType.CLI_READY
  details: {
    queueSize: number
    activeProcesses: number
    uptime: number
    memoryUsage: number
    framework: string
    enableHotkeys: boolean
    enableStreaming: boolean
    debug: boolean
  }
}

export interface CLIShutdownMessage extends SystemControlMessage {
  subtype: CLIMessageType.CLI_SHUTDOWN
  graceful: boolean
}

/**
 * User interaction messages
 */
export interface CLIUserInputMessage extends UserInputMessage {
  subtype: CLIMessageType.CLI_USER_INPUT
  mode: 'interactive' | 'command' | 'streaming'
  sourceType: 'keyboard' | 'paste' | 'programmatic'
}

export interface CLICommandRequestedMessage extends UserInputMessage {
  subtype: CLIMessageType.CLI_COMMAND_REQUESTED
  args: string[]
  flags: Record<string, string | boolean>
}

/**
 * UI state change messages
 */
export interface CLIModeChangedMessage extends SystemStatusMessage {
  subtype: CLIMessageType.CLI_MODE_CHANGED
  previousMode: 'interactive' | 'command' | 'streaming'
  newMode: 'interactive' | 'command' | 'streaming'
  reason?: string
}

export interface CLIPromptUpdatedMessage extends SystemStatusMessage {
  subtype: CLIMessageType.CLI_PROMPT_UPDATED
  oldPrompt: string
  newPrompt: string
  context: {
    provider?: string
    model?: string
    mode?: string
  }
}

/**
 * Display messages
 */
export interface CLIMessageReceivedMessage extends AgentOutputMessage {
  subtype: CLIMessageType.CLI_MESSAGE_RECEIVED
  messageType: 'info' | 'warning' | 'error' | 'success' | 'status' | 'streaming' | 'complete'
  display: {
    target: 'main' | 'status' | 'debug'
    timestamp: Date
    formatted: boolean
  }
}

export interface CLIProgressUpdateMessage extends SystemStatusMessage {
  subtype: CLIMessageType.CLI_PROGRESS_UPDATE
  phase: string
  progress: number // 0-100
  progressDetails?: string
  metadata?: {
    startTime?: Date
    estimatedCompletion?: Date
    step?: number
    totalSteps?: number
  }
}

/**
 * Streaming messages
 */
export interface CLIStreamingStartedMessage extends StreamStartMessage {
  subtype: CLIMessageType.CLI_STREAMING_STARTED
  source: 'agent' | 'command' | 'file'
  expectedDuration?: number
}

export interface CLIStreamingChunkMessage extends StreamDataMessage {
  subtype: CLIMessageType.CLI_STREAMING_CHUNK
  content: string
  size: number
}

export interface CLIStreamingCompleteMessage extends StreamEndMessage {
  subtype: CLIMessageType.CLI_STREAMING_COMPLETE
  duration: number
  finalMessage?: string
}

export interface CLIStreamingCancelledMessage extends StreamErrorMessage {
  subtype: CLIMessageType.CLI_STREAMING_CANCELLED
  reason: string
  partialChunks: number
  userInitiated: boolean
}

/**
 * Control messages
 */
export interface CLICancelRequestedMessage extends UserInterruptMessage {
  subtype: CLIMessageType.CLI_CANCEL_REQUESTED
  reason: 'user_escape' | 'user_interrupt' | 'shutdown' | 'timeout'
  immediate: boolean
  context?: {
    currentOperation?: string
    streamId?: string
    commandId?: string
  }
}

export interface CLIErrorOccurredMessage extends AgentErrorMessage {
  subtype: CLIMessageType.CLI_ERROR_OCCURRED
  context: 'cli' | 'agent' | 'command' | 'stream' | 'system'
  userVisible: boolean
  metadata?: {
    operation?: string
    stackTrace?: string
    additionalInfo?: Record<string, unknown>
  }
}

/**
 * Union type of all CLI messages
 */
export type CLIMessage =
  | CLIReadyMessage
  | CLIShutdownMessage
  | CLIUserInputMessage
  | CLICommandRequestedMessage
  | CLIModeChangedMessage
  | CLIPromptUpdatedMessage
  | CLIMessageReceivedMessage
  | CLIProgressUpdateMessage
  | CLIStreamingStartedMessage
  | CLIStreamingChunkMessage
  | CLIStreamingCompleteMessage
  | CLIStreamingCancelledMessage
  | CLICancelRequestedMessage
  | CLIErrorOccurredMessage

/**
 * CLI Message Factory for creating typed CLI messages
 */
export namespace CLIMessageFactory {
  export function generateId(): string {
    return `cli_${Date.now()}_${Math.random().toString(36).slice(2, 8)}`
  }

  export function createReadyMessage(config: {
    framework: string
    enableHotkeys: boolean
    enableStreaming: boolean
    debug: boolean
  }): CLIReadyMessage {
    return {
      id: CLIMessageFactory.generateId(),
      type: MessageType.SYSTEM_STATUS,
      subtype: CLIMessageType.CLI_READY,
      timestamp: new Date(),
      priority: MessagePriority.NORMAL,
      status: 'idle',
      details: {
        queueSize: 0,
        activeProcesses: 0,
        uptime: Date.now(),
        memoryUsage: process.memoryUsage().heapUsed,
        framework: config.framework,
        enableHotkeys: config.enableHotkeys,
        enableStreaming: config.enableStreaming,
        debug: config.debug,
      },
    }
  }

  export function createUserInputMessage(
    input: string,
    mode: CLIUserInputMessage['mode'],
    sourceType: CLIUserInputMessage['sourceType'] = 'keyboard'
  ): CLIUserInputMessage {
    return {
      id: CLIMessageFactory.generateId(),
      type: MessageType.USER_INPUT,
      subtype: CLIMessageType.CLI_USER_INPUT,
      timestamp: new Date(),
      priority: MessagePriority.NORMAL,
      input,
      raw: false,
      source: sourceType === 'keyboard' ? 'stdin' : sourceType === 'paste' ? 'cli' : 'api',
      mode,
      sourceType,
    }
  }

  export function createModeChangedMessage(
    previousMode: CLIModeChangedMessage['previousMode'],
    newMode: CLIModeChangedMessage['newMode'],
    reason?: string
  ): CLIModeChangedMessage {
    return {
      id: CLIMessageFactory.generateId(),
      type: MessageType.SYSTEM_STATUS,
      subtype: CLIMessageType.CLI_MODE_CHANGED,
      timestamp: new Date(),
      priority: MessagePriority.NORMAL,
      status: 'idle',
      details: {
        queueSize: 0,
        activeProcesses: 0,
        uptime: 0,
        memoryUsage: 0,
      },
      previousMode,
      newMode,
      reason,
    }
  }

  export function createMessageReceivedMessage(
    content: string,
    messageType: CLIMessageReceivedMessage['messageType'],
    target: CLIMessageReceivedMessage['display']['target'] = 'main'
  ): CLIMessageReceivedMessage {
    return {
      id: CLIMessageFactory.generateId(),
      type: MessageType.AGENT_OUTPUT,
      subtype: CLIMessageType.CLI_MESSAGE_RECEIVED,
      timestamp: new Date(),
      priority: MessagePriority.NORMAL,
      content,
      format: 'text',
      streaming: false,
      messageType,
      display: {
        target,
        timestamp: new Date(),
        formatted: false,
      },
    }
  }

  export function createProgressUpdateMessage(
    phase: string,
    progress: number,
    details?: string
  ): CLIProgressUpdateMessage {
    return {
      id: CLIMessageFactory.generateId(),
      type: MessageType.SYSTEM_STATUS,
      subtype: CLIMessageType.CLI_PROGRESS_UPDATE,
      timestamp: new Date(),
      priority: MessagePriority.NORMAL,
      status: 'processing',
      details: {
        queueSize: 0,
        activeProcesses: 1,
        uptime: 0,
        memoryUsage: 0,
      },
      phase,
      progress,
      progressDetails: details,
    }
  }

  export function createStreamingStartedMessage(
    streamId: string,
    source: CLIStreamingStartedMessage['source'] = 'agent'
  ): CLIStreamingStartedMessage {
    return {
      id: CLIMessageFactory.generateId(),
      type: MessageType.STREAM_START,
      subtype: CLIMessageType.CLI_STREAMING_STARTED,
      timestamp: new Date(),
      priority: MessagePriority.NORMAL,
      streamId,
      expectedSize: undefined,
      metadata: { source },
      source,
    }
  }

  export function createStreamingChunkMessage(
    streamId: string,
    content: string,
    sequence: number,
    isLast = false
  ): CLIStreamingChunkMessage {
    return {
      id: CLIMessageFactory.generateId(),
      type: MessageType.STREAM_DATA,
      subtype: CLIMessageType.CLI_STREAMING_CHUNK,
      timestamp: new Date(),
      priority: MessagePriority.NORMAL,
      streamId,
      data: content,
      sequence,
      isLast,
      content,
      size: content.length,
    }
  }

  export function createStreamingCompleteMessage(
    streamId: string,
    totalChunks: number,
    _totalSize: number,
    duration: number,
    finalMessage?: string
  ): CLIStreamingCompleteMessage {
    return {
      id: CLIMessageFactory.generateId(),
      type: MessageType.STREAM_END,
      subtype: CLIMessageType.CLI_STREAMING_COMPLETE,
      timestamp: new Date(),
      priority: MessagePriority.NORMAL,
      streamId,
      totalMessages: totalChunks,
      success: true,
      duration,
      finalMessage,
    }
  }

  export function createStreamingCancelledMessage(
    streamId: string,
    reason: string,
    partialChunks: number,
    userInitiated = false
  ): CLIStreamingCancelledMessage {
    return {
      id: CLIMessageFactory.generateId(),
      type: MessageType.STREAM_ERROR,
      subtype: CLIMessageType.CLI_STREAMING_CANCELLED,
      timestamp: new Date(),
      priority: MessagePriority.HIGH,
      streamId,
      error: create('STREAM_CANCELLED', reason, 'SYSTEM'),
      sequence: partialChunks,
      reason,
      partialChunks,
      userInitiated,
    }
  }

  export function createCancelRequestedMessage(
    reason: CLICancelRequestedMessage['reason'],
    immediate = false,
    signal?: string
  ): CLICancelRequestedMessage {
    return {
      id: CLIMessageFactory.generateId(),
      type: MessageType.USER_INTERRUPT,
      subtype: CLIMessageType.CLI_CANCEL_REQUESTED,
      timestamp: new Date(),
      priority: MessagePriority.HIGH,
      signal: signal || 'USER_CANCEL',
      graceful: !immediate,
      reason,
      immediate,
    }
  }

  export function createErrorOccurredMessage(
    error: QiError,
    context: CLIErrorOccurredMessage['context'],
    recoverable = true,
    userVisible = true
  ): CLIErrorOccurredMessage {
    return {
      id: CLIMessageFactory.generateId(),
      type: MessageType.AGENT_ERROR,
      subtype: CLIMessageType.CLI_ERROR_OCCURRED,
      timestamp: new Date(),
      priority: MessagePriority.HIGH,
      error,
      recoverable,
      suggestions: [],
      context,
      userVisible,
    }
  }
  export function createShutdownMessage(graceful = true): CLIShutdownMessage {
    return {
      id: CLIMessageFactory.generateId(),
      type: MessageType.SYSTEM_CONTROL,
      subtype: CLIMessageType.CLI_SHUTDOWN,
      timestamp: new Date(),
      priority: graceful ? MessagePriority.NORMAL : MessagePriority.HIGH,
      action: 'shutdown',
      immediate: !graceful,
      reason: graceful ? 'Graceful shutdown' : 'Immediate shutdown',
      graceful,
    }
  }
}
