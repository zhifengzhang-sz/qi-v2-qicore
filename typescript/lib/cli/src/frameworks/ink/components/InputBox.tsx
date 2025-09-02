/**
 * Input Box Component for Ink CLI
 *
 * Handles user input with proper parsing and command detection
 */

import { useState, useTransition, useRef, useEffect, useMemo } from 'react'
import { Box, Text, useInput, useApp } from 'ink'
import TextInput from 'ink-text-input'
import { LoadingSpinner } from './LoadingIndicator'
import { HybridTextInput } from './HybridTextInput'
import { useHybridHistory } from '../../hybrid/hooks/useHybridHistory'
import type { AppState, AppSubState } from '../../../abstractions/index'
import { createDebugLogger } from '../../../utils'

// QiCore debug logging
const logger = createDebugLogger('InputBox')
const debugLog = (message: string) => {
  logger.trace(message)
}

interface InputBoxProps {
  state: AppState
  subState?: AppSubState
  onSubmit: (input: string) => void
  onStateChange?: () => void
  onCommand?: (command: string, args: string[]) => void
  onCancel?: () => void
  onClear?: () => void
  placeholder?: string
  framework?: any // Framework instance for listening to events
  currentInput?: string // Current input text controlled by framework
  onSuggestions?: (suggestions: Array<{ command: string; description: string }>) => void
}

// Claude Code-style command suggestions
const COMMAND_SUGGESTIONS = [
  { command: '/help', description: 'Show available commands' },
  { command: '/clear', description: 'Clear conversation history' },
  { command: '/model', description: 'Switch AI model' },
  { command: '/provider', description: 'Switch AI provider' },
  { command: '/status', description: 'Show system status' },
  { command: '/tokens', description: 'Set max tokens limit' },
  { command: '/config', description: 'View configuration' },
  { command: '/exit', description: 'Exit the application' },
  { command: '/permission', description: 'Demo permission dialog' },
]

export function InputBox({
  state,
  subState,
  onSubmit,
  onStateChange,
  onCommand,
  onCancel,
  onClear,
  placeholder = 'Enter command or prompt...',
  framework,
  currentInput,
  onSuggestions,
}: InputBoxProps) {
  // Use currentInput from framework, fallback to local state for backward compatibility
  const [localInput, setLocalInput] = useState('')
  const [hybridInput, setHybridInput] = useState('')
  const [hybridCursor, setHybridCursor] = useState(0)
  const [selectedSuggestionIndex, setSelectedSuggestionIndex] = useState(0)
  const [isPending, startTransition] = useTransition()
  const { exit } = useApp()

  // CRITICAL FIX: Use ref to preserve cursor position across React re-renders
  // This prevents cursor reset when mode changes trigger component re-renders
  const cursorPositionRef = useRef(0)

  const isDisabled = state === 'busy' || isPending

  // Check if we're in hybrid mode (enhanced with Claude Code navigation)
  const isHybridMode =
    framework &&
    framework.constructor &&
    framework.constructor.name === 'HybridCLIFramework' &&
    framework.isHybridEnabled

  // Check if we're in a TTY environment for hybrid mode
  const isHybridTTYMode = isHybridMode && process.stdin.isTTY

  // DEBUG: Log component renders and state changes
  debugLog(
    `🔍 InputBox render: hybrid=${isHybridMode}, tty=${isHybridTTYMode}, state=${state}, cursor=${hybridCursor}, ref=${cursorPositionRef.current}`
  )

  // In hybrid mode, use hybrid input; otherwise use currentInput or localInput
  const input = isHybridMode ? hybridInput : (currentInput ?? localInput)

  // Listen to global framework events for input clearing (avoids useInput conflict)
  useEffect(() => {
    if (!framework) return

    const handleClearInput = () => {
      debugLog('🧹 InputBox: handleClearInput called - CURSOR WILL BE RESET TO 0')
      setLocalInput('')
      if (isHybridMode) {
        setHybridInput('')
        cursorPositionRef.current = 0 // Update ref
        setHybridCursor(0)
        debugLog('🧹 InputBox: Hybrid cursor cleared to 0')
      }
      if (onClear) {
        onClear()
      }
    }

    const handleInputUpdate = (data: { text: string; cursorPosition: number }) => {
      debugLog(
        `📝 InputBox: handleInputUpdate called - text:'${data.text}', cursor:${data.cursorPosition}, hybrid:${isHybridMode}`
      )
      if (isHybridMode) {
        setHybridInput(data.text)
        setHybridCursor(data.cursorPosition)
        cursorPositionRef.current = data.cursorPosition // Update ref too
        debugLog('📝 InputBox: Updated hybrid input and cursor from framework')
      }
    }

    // Event listeners removed - input clearing handled through direct callbacks
    // Hybrid mode input updates handled through StateManager subscriptions
  }, [framework, onClear, isHybridMode])

  const handleSubmit = (value: string) => {
    if (value.trim()) {
      startTransition(() => {
        const trimmedValue = value.trim()

        // Add to history in hybrid mode
        if (isHybridMode && hybridHistory) {
          hybridHistory.addToHistory(trimmedValue)
          hybridHistory.resetHistory() // Reset history navigation
        }

        // Check if it's a command
        if (trimmedValue.startsWith('/') && onCommand) {
          const parts = trimmedValue.slice(1).split(' ')
          const command = parts[0]
          const args = parts.slice(1)
          if (command) {
            onCommand(command, args)
          }
        } else {
          onSubmit(trimmedValue)
        }

        // Clear both local and hybrid input
        debugLog('🚀 InputBox: handleSubmit - clearing input and cursor')
        setLocalInput('')
        if (isHybridMode) {
          setHybridInput('')
          cursorPositionRef.current = 0 // Update ref
          setHybridCursor(0)
          debugLog('🚀 InputBox: Hybrid cursor reset after submit')
        }
      })
    }
  }

  const getPromptPrefix = () => {
    if (state === 'busy') {
      return '⏳'
    }

    const prefixes = {
      planning: '📋',
      editing: '✏️',
      generic: '💬',
    }

    return prefixes[subState || 'generic']
  }

  // Memoize suggestions calculation to prevent infinite loops
  const suggestions = useMemo(() => {
    if (!input.startsWith('/')) return []

    // Show all commands when just typing '/'
    if (input.length === 1) {
      return COMMAND_SUGGESTIONS.slice(0, 6) // Show more commands initially
    }

    // Filter commands when typing more characters
    const query = input.toLowerCase()
    return COMMAND_SUGGESTIONS.filter((suggestion) =>
      suggestion.command.toLowerCase().startsWith(query)
    ).slice(0, 6) // Show up to 6 suggestions
  }, [input])

  // Reset selection when suggestions change
  useEffect(() => {
    setSelectedSuggestionIndex(0)
  }, [suggestions.length])

  // Handle keyboard navigation for command suggestions (skip in hybrid mode)
  useInput(
    (input, key) => {
      // In hybrid mode, let HybridTextInput handle all input
      if (isHybridMode) {
        return
      }

      // CRITICAL FIX: Block Shift+Tab to prevent cursor advancement in regular mode
      if (key.tab && key.shift) {
        logger.trace('Blocking Shift+Tab to prevent cursor advancement in regular mode')
        // Mode cycling happens elsewhere - we just need to prevent the tab character insertion
        return
      }

      // Regular mode: handle command suggestions
      if (suggestions.length === 0) return

      if (key.upArrow) {
        setSelectedSuggestionIndex((prev) => (prev <= 0 ? suggestions.length - 1 : prev - 1))
        return
      }

      if (key.downArrow) {
        setSelectedSuggestionIndex((prev) => (prev >= suggestions.length - 1 ? 0 : prev + 1))
        return
      }

      if (key.tab && suggestions.length > 0) {
        // Tab to select current suggestion
        const selectedCommand = suggestions[selectedSuggestionIndex]
        if (selectedCommand) {
          setLocalInput(selectedCommand.command + ' ')
          setSelectedSuggestionIndex(0)
        }
        return
      }

      if (key.return && suggestions.length > 0 && input.trim().startsWith('/')) {
        // Enter with suggestions visible - auto-complete first suggestion if input is just '/'
        if (input.trim() === '/') {
          const selectedCommand = suggestions[selectedSuggestionIndex]
          if (selectedCommand) {
            setLocalInput(selectedCommand.command + ' ')
            setSelectedSuggestionIndex(0)
            return
          }
        }
      }
    },
    { isActive: !isDisabled && !isHybridMode }
  ) // Disable completely in hybrid mode

  // Notify parent about suggestions changes (include selected index)
  useEffect(() => {
    if (onSuggestions) {
      onSuggestions(
        suggestions.map((suggestion, index) => ({
          ...suggestion,
          selected: index === selectedSuggestionIndex,
        }))
      )
    }
  }, [suggestions, selectedSuggestionIndex, onSuggestions])

  // History management for hybrid mode
  const hybridHistory = useHybridHistory({
    onSetInput: (value: string) => {
      setHybridInput(value)
      cursorPositionRef.current = value.length // Update ref
      setHybridCursor(value.length) // Move cursor to end when setting from history
    },
    currentInput: hybridInput,
  })

  // Enhanced hybrid mode input handler that syncs with cursor changes
  const handleHybridInputChange = (newValue: string) => {
    setHybridInput(newValue)
    // Reset history navigation when user types
    if (hybridHistory.historyIndex > 0) {
      hybridHistory.resetHistory()
    }
    // Reset cursor end state when user types
    if (framework && framework.resetCursorEndState) {
      framework.resetCursorEndState()
    }
  }

  // CRITICAL FIX: Enhanced cursor change handler that preserves position across mode changes
  const handleHybridCursorChange = (newOffset: number) => {
    logger.trace(
      `CURSOR CHANGE: old:${hybridCursor} → new:${newOffset}, ref:${cursorPositionRef.current}`
    )
    debugLog(
      `🔧 InputBox: Cursor change requested - old:${hybridCursor}, new:${newOffset}, ref:${cursorPositionRef.current}`
    )
    cursorPositionRef.current = newOffset // Always update ref
    setHybridCursor(newOffset) // Update state for re-renders
  }

  // Get current mode for effect dependency
  const currentMode = framework?.getMode?.()

  // CRITICAL FIX: Restore cursor position from ref after mode changes
  // This ensures cursor position survives React re-renders triggered by framework state changes
  useEffect(() => {
    if (isHybridMode && cursorPositionRef.current !== hybridCursor) {
      logger.trace(
        `RESTORING CURSOR: mode:${currentMode}, ref:${cursorPositionRef.current} → state:${hybridCursor}`
      )
      debugLog(
        `🔧 InputBox: Restoring cursor from ref after re-render - mode:${currentMode}, ref:${cursorPositionRef.current}, state:${hybridCursor}`
      )
      // Restore cursor position from ref (which survives re-renders)
      setHybridCursor(cursorPositionRef.current)
    }
  }, [isHybridMode, currentMode]) // Re-run only when mode changes, not hybridCursor

  // History navigation handlers for hybrid mode
  const handleHistoryUp = () => {
    hybridHistory.onHistoryUp()
  }

  const handleHistoryDown = () => {
    hybridHistory.onHistoryDown()
  }

  // Command suggestion navigation handlers for hybrid mode
  const handleCommandSuggestionUp = () => {
    if (suggestions.length === 0) return
    setSelectedSuggestionIndex((prev) => (prev <= 0 ? suggestions.length - 1 : prev - 1))
  }

  const handleCommandSuggestionDown = () => {
    if (suggestions.length === 0) return
    setSelectedSuggestionIndex((prev) => (prev >= suggestions.length - 1 ? 0 : prev + 1))
  }

  const handleCommandSuggestionAccept = () => {
    if (suggestions.length > 0) {
      const selectedCommand = suggestions[selectedSuggestionIndex]
      if (selectedCommand) {
        setHybridInput(selectedCommand.command + ' ')
        setHybridCursor(selectedCommand.command.length + 1)
        setSelectedSuggestionIndex(0)
      }
    }
  }

  // In hybrid mode, use proper HybridTextInput following Claude Code architecture
  if (isHybridMode) {
    return (
      <Box flexDirection="column">
        <Box>
          <Text color="#007acc">{getPromptPrefix()} </Text>
          {!isDisabled ? (
            <>
              <HybridTextInput
                value={hybridInput}
                onChange={handleHybridInputChange}
                onSubmit={handleSubmit}
                onHistoryUp={handleHistoryUp}
                onHistoryDown={handleHistoryDown}
                onCommandSuggestionUp={handleCommandSuggestionUp}
                onCommandSuggestionDown={handleCommandSuggestionDown}
                onCommandSuggestionAccept={handleCommandSuggestionAccept}
                hasCommandSuggestions={suggestions.length > 0}
                placeholder={placeholder}
                focus={true}
                framework={framework}
                cursorOffset={hybridCursor}
                onCursorOffsetChange={handleHybridCursorChange}
                columns={80}
              />
              {!isHybridTTYMode && (
                <Box paddingTop={1}>
                  <Text dimColor>📝 Using fallback input mode (no raw terminal)</Text>
                </Box>
              )}
              {isHybridTTYMode && (
                <Box paddingTop={1}>
                  <Text dimColor>
                    🔧 Hybrid Mode: Raw terminal input active (Claude Code navigation enabled)
                  </Text>
                </Box>
              )}
            </>
          ) : (
            <Box>
              <LoadingSpinner />
              <Text color="#e0e0e0" dimColor>
                {' '}
                Please wait...
              </Text>
            </Box>
          )}
        </Box>
      </Box>
    )
  }

  return (
    <Box flexDirection="column">
      <Box>
        <Text color="#007acc">{getPromptPrefix()} </Text>
        {!isDisabled ? (
          <TextInput
            value={input}
            onChange={currentInput ? () => {} : setLocalInput} // No-op if controlled by framework
            onSubmit={handleSubmit}
            placeholder={placeholder}
            focus={true}
          />
        ) : (
          <Box>
            <LoadingSpinner />
            <Text color="#e0e0e0" dimColor>
              {' '}
              Please wait...
            </Text>
          </Box>
        )}
      </Box>
    </Box>
  )
}
