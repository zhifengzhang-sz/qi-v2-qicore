/**
 * Main Layout Component for Ink CLI
 *
 * Combines all UI components into a cohesive layout
 */

import React, { memo } from 'react'
import { Box, Text } from 'ink'
import Gradient from 'ink-gradient'
import BigText from 'ink-big-text'
import { InputBox } from './InputBox'
import { OutputDisplay, type OutputMessage } from './OutputDisplay'
import { LoadingIndicator } from './LoadingIndicator'
import { PermissionDialog, type PermissionRequest } from './PermissionDialog'
import type { AppState, AppSubState } from '../../../abstractions/index'
import { styles, defaultTheme, getInputBorderColor, textStyles } from '../styles/theme'

interface MainLayoutProps {
  state: AppState
  subState?: AppSubState
  taskName?: string
  messages: OutputMessage[]
  onInput: (input: string) => void
  onStateChange?: () => void
  onCommand?: (command: string, args: string[]) => void
  onCancel?: () => void
  onClear?: () => void
  provider?: string
  model?: string
  mode?: string
  isProcessing?: boolean
  currentPhase?: string
  framework?: any
  currentInput?: string // Add current input text from framework
  permissionRequest?: PermissionRequest | null
  onPermissionApprove?: (requestId: string, remember?: boolean) => void
  onPermissionDeny?: (requestId: string, remember?: boolean) => void
  onPermissionDismiss?: () => void
}

export const MainLayout = memo(function MainLayout({
  state,
  subState,
  taskName,
  messages,
  onInput,
  onStateChange,
  onCommand,
  onCancel,
  onClear,
  provider = 'ollama',
  model = 'qwen3:0.6b',
  mode = 'interactive',
  isProcessing = false,
  currentPhase = '',
  framework,
  currentInput,
  permissionRequest = null,
  onPermissionApprove,
  onPermissionDeny,
  onPermissionDismiss,
}: MainLayoutProps) {
  const [suggestions, setSuggestions] = React.useState<
    Array<{ command: string; description: string; selected?: boolean }>
  >([])

  const handleSuggestions = React.useCallback(
    (newSuggestions: Array<{ command: string; description: string; selected?: boolean }>) => {
      setSuggestions(newSuggestions)
    },
    []
  )
  return (
    <Box flexDirection="column" height="100%" width="100%">
      {/* Header - Claude Code style */}
      <Box {...styles.header}>
        <Text color="#007acc" bold>
          █ Qi CLI
        </Text>
        <Text color="dim" dimColor>
          {' '}
          – AI-powered development assistant
        </Text>
      </Box>

      {/* Main Content Area */}
      <Box {...styles.content}>
        <OutputDisplay messages={messages} />

        {/* Permission Dialog Overlay */}
        {permissionRequest && (
          <PermissionDialog
            request={permissionRequest}
            onApprove={onPermissionApprove || (() => {})}
            onDeny={onPermissionDeny || (() => {})}
            onDismiss={onPermissionDismiss || (() => {})}
          />
        )}
      </Box>

      {/* Input Area */}
      <Box
        {...styles.inputContainer}
        borderColor={getInputBorderColor(state === 'busy' ? 'busy' : 'ready')}
      >
        <InputBox
          state={state}
          subState={subState}
          onSubmit={onInput}
          onStateChange={onStateChange}
          onCommand={onCommand}
          onCancel={onCancel}
          onClear={onClear}
          framework={framework}
          currentInput={currentInput}
          onSuggestions={handleSuggestions}
        />
      </Box>

      {/* Status Line - Claude Code style with conditional command suggestions */}
      <Box {...styles.statusLine}>
        {suggestions.length > 0 && state !== 'busy' ? (
          /* Show command suggestions instead of normal status when typing commands */
          <Box flexDirection="column" width="100%">
            {suggestions.map((suggestion, index) => (
              <Box key={suggestion.command}>
                <Text
                  color={suggestion.selected ? '#ffffff' : '#4caf50'}
                  backgroundColor={suggestion.selected ? '#007acc' : undefined}
                  bold={suggestion.selected}
                >
                  {suggestion.selected ? '► ' : '  '}
                  {suggestion.command}
                </Text>
                <Text
                  color={suggestion.selected ? '#ffffff' : 'dim'}
                  dimColor={!suggestion.selected}
                  backgroundColor={suggestion.selected ? '#007acc' : undefined}
                >
                  {' '}
                  – {suggestion.description}
                </Text>
              </Box>
            ))}
            {/* Add navigation help text */}
            <Box marginTop={1}>
              <Text color="dim" dimColor>
                ↑↓ navigate • Tab/Enter select • Esc cancel
              </Text>
            </Box>
          </Box>
        ) : (
          /* Normal status line */
          <Box {...styles.statusLeft}>
            <Text color="#ff6b35" bold>
              {provider}
            </Text>
            <Text color="dim" dimColor>
              {' '}
              →{' '}
            </Text>
            <Text color="#4caf50" bold>
              {model}
            </Text>
            <Text color="dim" dimColor>
              {' '}
              •{' '}
            </Text>
            <Text color="#007acc">{mode}</Text>
          </Box>
        )}
        {/* Only show right status when not showing command suggestions */}
        {!(suggestions.length > 0 && state !== 'busy') &&
          (isProcessing ? (
            <Box {...styles.statusRight}>
              <LoadingIndicator
                message={currentPhase || 'Processing'}
                showAnimation={true}
                color="#ff9800"
              />
              <Text color="dim" dimColor>
                {' '}
                •{' '}
              </Text>
              <Text color="#2196f3">Esc to cancel</Text>
            </Box>
          ) : (
            <Box {...styles.statusRight}>
              <Text color="dim" dimColor>
                Shift+Tab mode • Ctrl+C clear • Ctrl+D exit
              </Text>
            </Box>
          ))}
      </Box>
    </Box>
  )
})
