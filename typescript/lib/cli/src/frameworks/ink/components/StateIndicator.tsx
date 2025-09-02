/**
 * State Indicator Component for Ink CLI
 *
 * Shows current application state with visual indicators
 */

import { memo } from 'react'
import { Box, Text } from 'ink'
import type { AppState, AppSubState } from '../../../abstractions/index'

interface StateIndicatorProps {
  state: AppState
  subState?: AppSubState
  taskName?: string
  provider?: string
  model?: string
  mode?: string
  isProcessing?: boolean
  currentPhase?: string
  progress?: number
}

const STATE_COLORS = {
  busy: 'yellow',
  ready: 'green',
} as const

const SUB_STATE_LABELS = {
  planning: '📋 Planning',
  editing: '✏️  Editing',
  generic: '💬 Generic',
} as const

const MODE_LABELS = {
  interactive: '💬 Interactive',
  command: '⚡ Command',
  streaming: '🌊 Streaming',
} as const

export const StateIndicator = memo(function StateIndicator({
  state,
  subState,
  taskName,
  provider = 'ollama',
  model = 'qwen3:0.6b',
  mode = 'interactive',
  isProcessing = false,
  currentPhase = '',
  progress = 0,
}: StateIndicatorProps) {
  const getModeDisplay = () => {
    return MODE_LABELS[mode as keyof typeof MODE_LABELS] || '💬 Interactive'
  }

  const getProgressDisplay = () => {
    if (isProcessing && currentPhase) {
      const percentage = Math.round(progress * 100)
      return `⏳ ${currentPhase} ${percentage}%`
    }
    return null
  }

  return (
    <Box flexDirection="column" alignItems="flex-end">
      {/* Provider and Model Info */}
      <Box>
        <Text color="cyan">{provider}:</Text>
        <Text color="white" bold>
          {' '}
          {model}
        </Text>
      </Box>

      {/* Mode and Status */}
      <Box>
        {getProgressDisplay() ? (
          <Text color="yellow">{getProgressDisplay()}</Text>
        ) : (
          <>
            <Text color="green">{getModeDisplay()}</Text>
            <Text color="dim" dimColor>
              {' '}
              (Shift+Tab to cycle)
            </Text>
          </>
        )}
      </Box>
    </Box>
  )
})
