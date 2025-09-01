/**
 * useHybridHistory Hook - Following Claude Code's History Pattern
 *
 * Implements Claude Code's history navigation behavior:
 * 1. Store history in memory (simplified version)
 * 2. Navigate up/down through history
 * 3. Preserve current input when navigating
 * 4. Reset to current input when going back down
 *
 * Based on Claude Code's useArrowKeyHistory.ts
 */

import { useState } from 'react'

interface UseHybridHistoryProps {
  onSetInput: (value: string) => void
  currentInput: string
}

interface UseHybridHistoryResult {
  onHistoryUp: () => void
  onHistoryDown: () => void
  addToHistory: (input: string) => void
  resetHistory: () => void
  historyIndex: number
}

// Simple in-memory history storage (could be enhanced to persist)
const historyStorage: string[] = []
const MAX_HISTORY_ITEMS = 100

export function useHybridHistory({
  onSetInput,
  currentInput,
}: UseHybridHistoryProps): UseHybridHistoryResult {
  const [historyIndex, setHistoryIndex] = useState(0)
  const [lastTypedInput, setLastTypedInput] = useState('')

  // Add command to history (Claude Code pattern)
  const addToHistory = (command: string) => {
    const trimmed = command.trim()
    if (!trimmed) return

    // Don't add duplicates of the last command
    if (historyStorage[0] === trimmed) return

    // Add to beginning of history array
    historyStorage.unshift(trimmed)

    // Keep only MAX_HISTORY_ITEMS
    if (historyStorage.length > MAX_HISTORY_ITEMS) {
      historyStorage.splice(MAX_HISTORY_ITEMS)
    }
  }

  // Navigate up in history (Claude Code pattern)
  const onHistoryUp = () => {
    if (historyIndex < historyStorage.length) {
      // Save current input when starting history navigation
      if (historyIndex === 0 && currentInput.trim() !== '') {
        setLastTypedInput(currentInput)
      }

      const newIndex = historyIndex + 1
      setHistoryIndex(newIndex)

      // Set input to history item
      const historyItem = historyStorage[historyIndex]
      if (historyItem) {
        onSetInput(historyItem)
      }
    }
  }

  // Navigate down in history (Claude Code pattern)
  const onHistoryDown = () => {
    if (historyIndex > 1) {
      // Move down in history
      const newIndex = historyIndex - 1
      setHistoryIndex(newIndex)

      const historyItem = historyStorage[newIndex - 1]
      if (historyItem) {
        onSetInput(historyItem)
      }
    } else if (historyIndex === 1) {
      // Return to original typed input
      setHistoryIndex(0)
      onSetInput(lastTypedInput)
    }
  }

  // Reset history navigation (Claude Code pattern)
  const resetHistory = () => {
    setLastTypedInput('')
    setHistoryIndex(0)
  }

  return {
    onHistoryUp,
    onHistoryDown,
    addToHistory,
    resetHistory,
    historyIndex,
  }
}
