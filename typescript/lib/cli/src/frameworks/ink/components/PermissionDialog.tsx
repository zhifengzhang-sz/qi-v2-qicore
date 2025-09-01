/**
 * Permission Dialog Component
 *
 * Inspired by Claude Code's permission request dialogs for tool usage approval.
 * Provides interactive prompts for confirming potentially sensitive operations.
 */

import React, { useState, useEffect } from 'react'
import { Box, Text, useInput } from 'ink'
import { defaultTheme, textStyles } from '../styles/theme'

interface PermissionRequest {
  id: string
  toolName: string
  action: string
  description: string
  risks?: string[]
  autoApprove?: boolean
}

interface PermissionDialogProps {
  request: PermissionRequest | null
  onApprove: (requestId: string, remember?: boolean) => void
  onDeny: (requestId: string, remember?: boolean) => void
  onDismiss: () => void
}

export function PermissionDialog({ request, onApprove, onDeny, onDismiss }: PermissionDialogProps) {
  const [selectedOption, setSelectedOption] = useState<'approve' | 'deny' | 'always' | 'never'>(
    'approve'
  )
  const [isVisible, setIsVisible] = useState(false)

  useEffect(() => {
    if (request) {
      setIsVisible(true)
      setSelectedOption('approve')
    } else {
      setIsVisible(false)
    }
  }, [request])

  useInput((input, key) => {
    if (!isVisible || !request) return

    if (key.leftArrow || key.rightArrow) {
      const options: ('approve' | 'deny' | 'always' | 'never')[] = [
        'approve',
        'deny',
        'always',
        'never',
      ]
      const currentIndex = options.indexOf(selectedOption)

      if (key.leftArrow) {
        const newIndex = currentIndex > 0 ? currentIndex - 1 : options.length - 1
        setSelectedOption(options[newIndex])
      } else if (key.rightArrow) {
        const newIndex = currentIndex < options.length - 1 ? currentIndex + 1 : 0
        setSelectedOption(options[newIndex])
      }
    }

    if (key.return) {
      switch (selectedOption) {
        case 'approve':
          onApprove(request.id, false)
          break
        case 'deny':
          onDeny(request.id, false)
          break
        case 'always':
          onApprove(request.id, true)
          break
        case 'never':
          onDeny(request.id, true)
          break
      }
      setIsVisible(false)
    }

    if (key.escape) {
      onDismiss()
      setIsVisible(false)
    }
  })

  if (!isVisible || !request) {
    return null
  }

  return (
    <Box flexDirection="column" padding={1} borderStyle="double" borderColor="yellow" marginY={1}>
      {/* Header */}
      <Box marginBottom={1}>
        <Text color="yellow" bold>
          🔐 Tool Permission Request
        </Text>
      </Box>

      {/* Tool information */}
      <Box flexDirection="column" marginBottom={1}>
        <Box>
          <Text color="cyan" bold>
            Tool:{' '}
          </Text>
          <Text color="white">{request.toolName}</Text>
        </Box>
        <Box>
          <Text color="cyan" bold>
            Action:{' '}
          </Text>
          <Text color="white">{request.action}</Text>
        </Box>
      </Box>

      {/* Description */}
      <Box marginBottom={1}>
        <Text color="white">{request.description}</Text>
      </Box>

      {/* Risks warning */}
      {request.risks && request.risks.length > 0 && (
        <Box flexDirection="column" marginBottom={1}>
          <Text color="red" bold>
            ⚠️ Potential risks:
          </Text>
          {request.risks.map((risk, index) => (
            <Box key={index} paddingLeft={2}>
              <Text color="red">• {risk}</Text>
            </Box>
          ))}
        </Box>
      )}

      {/* Options */}
      <Box justifyContent="space-between" marginBottom={1}>
        <OptionButton label="Approve" isSelected={selectedOption === 'approve'} color="green" />
        <OptionButton label="Deny" isSelected={selectedOption === 'deny'} color="red" />
        <OptionButton label="Always" isSelected={selectedOption === 'always'} color="blue" />
        <OptionButton label="Never" isSelected={selectedOption === 'never'} color="gray" />
      </Box>

      {/* Instructions */}
      <Box>
        <Text color="dim">← → to navigate • Enter to confirm • Esc to dismiss</Text>
      </Box>
    </Box>
  )
}

interface OptionButtonProps {
  label: string
  isSelected: boolean
  color: string
}

function OptionButton({ label, isSelected, color }: OptionButtonProps) {
  const bgColor = isSelected ? color : undefined
  const textColor = isSelected ? 'black' : color

  return (
    <Box
      paddingX={2}
      paddingY={0}
      borderStyle={isSelected ? 'single' : undefined}
      borderColor={color}
    >
      <Text color={textColor} backgroundColor={bgColor} bold={isSelected}>
        {label}
      </Text>
    </Box>
  )
}

/**
 * Helper function to create permission requests
 */
export function createPermissionRequest(
  toolName: string,
  action: string,
  description: string,
  risks?: string[]
): PermissionRequest {
  return {
    id: `perm-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
    toolName,
    action,
    description,
    risks,
  }
}

export type { PermissionRequest }
