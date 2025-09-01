/**
 * Output Display Component for Ink CLI
 *
 * Shows command results and system messages
 */

import React from 'react'
import { Box, Text, Newline } from 'ink'
import Spinner from 'ink-spinner'

interface OutputMessage {
  id: string
  content: string
  type:
    | 'info'
    | 'success'
    | 'error'
    | 'warning'
    | 'command'
    | 'response'
    | 'streaming'
    | 'complete'
    | 'status'
    | 'user'
    | 'assistant'
  timestamp: Date
  isProcessing?: boolean
  sender?: 'user' | 'assistant' | 'system'
}

interface OutputDisplayProps {
  messages: OutputMessage[]
  maxMessages?: number
}

const MESSAGE_COLORS = {
  info: '#2196f3',
  success: '#4caf50',
  error: '#f44336',
  warning: '#ff9800',
  command: '#007acc',
  response: '#e0e0e0',
  streaming: '#ff6b35',
  complete: '#4caf50',
  status: '#9e9e9e',
  user: '#007acc',
  assistant: '#e0e0e0',
} as const

const MESSAGE_PREFIXES = {
  info: 'ℹ️',
  success: '✅',
  error: '❌',
  warning: '⚠️',
  command: '▶️',
  response: '💬',
  streaming: '🌊',
  complete: '✅',
  status: 'ℹ️',
  user: '👤',
  assistant: '🤖',
} as const

// Sender labels for qi-prompt
const SENDER_LABELS = {
  user: 'You',
  assistant: 'Qi',
  system: 'System',
} as const

export function OutputDisplay({ messages, maxMessages = 50 }: OutputDisplayProps) {
  // Show only the most recent messages
  const displayMessages = messages.slice(-maxMessages)

  if (displayMessages.length === 0) {
    return (
      <Box paddingLeft={1} paddingRight={1}>
        <Text color="dim" dimColor>
          Welcome to Qi CLI! Type /help for available commands.
        </Text>
      </Box>
    )
  }

  return (
    <Box flexDirection="column" paddingLeft={1} paddingRight={1}>
      {displayMessages.map((message, index) => (
        <MessageItem key={`${message.id}-${index}`} message={message} />
      ))}
    </Box>
  )
}

function MessageItem({ message }: { message: OutputMessage }) {
  const color = MESSAGE_COLORS[message.type]
  const prefix = MESSAGE_PREFIXES[message.type]

  // Format timestamp
  const timeStr = message.timestamp.toLocaleTimeString('en-US', {
    hour12: false,
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit',
  })

  // Claude Code-style conversation rendering
  if (message.sender) {
    const senderLabel = SENDER_LABELS[message.sender]
    const senderColor =
      message.sender === 'user' ? '#007acc' : message.sender === 'assistant' ? '#4caf50' : '#9e9e9e'

    return (
      <Box flexDirection="column" marginBottom={1}>
        {/* Sender header with timestamp */}
        <Box marginBottom={0}>
          <Text color="dim" dimColor>
            [{timeStr}]
          </Text>
          <Text color={senderColor} bold>
            {senderLabel}:
          </Text>
        </Box>
        {/* Message content with proper indentation */}
        <Box paddingLeft={2}>
          {message.isProcessing ? (
            <Box>
              <Text color="cyan">
                <Spinner type="dots" />{' '}
              </Text>
              <Text color={color}>{message.content}</Text>
            </Box>
          ) : (
            <Text color={color}>{message.content}</Text>
          )}
        </Box>
      </Box>
    )
  }

  // Legacy message format for system messages
  return (
    <Box flexDirection="column" marginBottom={1}>
      <Box>
        <Text color="dim" dimColor>
          [{timeStr}]
        </Text>
        {message.isProcessing ? (
          <Text color="cyan">
            <Spinner type="dots" />{' '}
          </Text>
        ) : (
          <Text color={color}>{prefix}</Text>
        )}
        <Text color={color}>{message.content}</Text>
      </Box>
    </Box>
  )
}

// Counter to ensure unique message IDs
let messageCounter = 0

// Helper function to create output messages
export function createOutputMessage(
  content: string,
  type: OutputMessage['type'] = 'info',
  isProcessing: boolean = false
): OutputMessage {
  messageCounter++
  return {
    id: `msg-${Date.now()}-${messageCounter}-${Math.random().toString(36).substr(2, 9)}`,
    content,
    type,
    timestamp: new Date(),
    isProcessing,
  }
}

// Helper functions for Claude Code-style conversation messages
export function createUserMessage(content: string): OutputMessage {
  messageCounter++
  return {
    id: `msg-${Date.now()}-${messageCounter}-${Math.random().toString(36).substr(2, 9)}`,
    content,
    type: 'user',
    timestamp: new Date(),
    sender: 'user',
    isProcessing: false,
  }
}

export function createAssistantMessage(
  content: string,
  isProcessing: boolean = false
): OutputMessage {
  messageCounter++
  return {
    id: `msg-${Date.now()}-${messageCounter}-${Math.random().toString(36).substr(2, 9)}`,
    content,
    type: 'assistant',
    timestamp: new Date(),
    sender: 'assistant',
    isProcessing,
  }
}

export function createSystemMessage(content: string): OutputMessage {
  messageCounter++
  return {
    id: `msg-${Date.now()}-${messageCounter}-${Math.random().toString(36).substr(2, 9)}`,
    content,
    type: 'info',
    timestamp: new Date(),
    sender: 'system',
    isProcessing: false,
  }
}

export type { OutputMessage }
