/**
 * useHybridTextInput Hook - Following Claude Code's Design Pattern
 *
 * This hook implements Claude Code's exact input handling pattern:
 * 1. Immutable cursor operations
 * 2. Functional input mapping
 * 3. cursor.equals() for boundary detection
 * 4. Proper separation of concerns
 *
 * Based on analysis of Claude Code's useTextInput.ts
 */

import chalk from 'chalk'
import type { Key } from 'ink'
import { Cursor } from '../../../utils/Cursor'
import { createDebugLogger } from '../../../utils/DebugLogger'
import type { HybridCLIFramework } from '../HybridCLIFramework'

type MaybeCursor = undefined | Cursor
type InputHandler = (input: string) => MaybeCursor
type InputMapper = (input: string) => MaybeCursor

// Claude Code's mapInput pattern
function mapInput(input_map: Array<[string, InputHandler]>): InputMapper {
  return (input: string): MaybeCursor => {
    const handler = new Map(input_map).get(input) ?? (() => undefined)
    return handler(input)
  }
}

interface UseHybridTextInputProps {
  value: string
  onChange: (value: string) => void
  onSubmit?: (value: string) => void
  onHistoryUp?: () => void
  onHistoryDown?: () => void
  onCommandSuggestionUp?: () => void
  onCommandSuggestionDown?: () => void
  onCommandSuggestionAccept?: () => void
  hasCommandSuggestions?: boolean
  columns: number
  cursorOffset: number
  onCursorOffsetChange: (offset: number) => void
  framework?: HybridCLIFramework
}

interface UseHybridTextInputResult {
  renderedValue: string
  onInput: (input: string, key: Key) => void
}

export function useHybridTextInput({
  value,
  onChange,
  onSubmit,
  onHistoryUp,
  onHistoryDown,
  onCommandSuggestionUp,
  onCommandSuggestionDown,
  onCommandSuggestionAccept,
  hasCommandSuggestions = false,
  columns,
  cursorOffset,
  onCursorOffsetChange,
  framework,
}: UseHybridTextInputProps): UseHybridTextInputResult {
  // Create debug logger
  const logger = createDebugLogger('useHybridTextInput')

  // Create cursor from current state (Claude Code pattern)
  const cursor = Cursor.fromText(value, columns, cursorOffset)

  // Enhanced navigation pattern with command suggestions support
  function upOrHistoryUp() {
    // If command suggestions are visible, navigate through them first
    if (hasCommandSuggestions) {
      onCommandSuggestionUp?.()
      return cursor // Don't move cursor when navigating suggestions
    }

    // Step 1: Try cursor movement first
    const cursorUp = cursor.up()

    // Step 2: Check if cursor movement failed
    if (cursorUp.equals(cursor)) {
      // Step 3: Cursor couldn't move - trigger history
      onHistoryUp?.()
    }

    // Step 4: Always return cursor (moved or unchanged)
    return cursorUp
  }

  function downOrHistoryDown() {
    // If command suggestions are visible, navigate through them first
    if (hasCommandSuggestions) {
      onCommandSuggestionDown?.()
      return cursor // Don't move cursor when navigating suggestions
    }

    // Step 1: Try cursor movement first
    const cursorDown = cursor.down()

    // Step 2: Check if cursor movement failed
    if (cursorDown.equals(cursor)) {
      // Step 3: Cursor couldn't move - trigger history
      onHistoryDown?.()
    }

    // Step 4: Always return cursor (moved or unchanged)
    return cursorDown
  }

  // Handle Enter key
  function handleEnter() {
    onSubmit?.(value)
    return cursor // Return current cursor, don't change text
  }

  // Claude Code's Ctrl key mappings
  const handleCtrl = mapInput([
    ['a', () => cursor.startOfLine()], // Ctrl+A
    ['e', () => cursor.endOfLine()], // Ctrl+E
    ['f', () => cursor.right()], // Ctrl+F
    ['b', () => cursor.left()], // Ctrl+B
    ['d', () => cursor.del()], // Ctrl+D
    ['h', () => cursor.backspace()], // Ctrl+H
    ['k', () => cursor.deleteToLineEnd()], // Ctrl+K
    ['u', () => cursor.deleteToLineStart()], // Ctrl+U
    ['w', () => cursor.deleteWordBefore()], // Ctrl+W
  ])

  // Claude Code's Meta key mappings
  const handleMeta = mapInput([
    ['f', () => cursor.nextWord()], // Alt+F
    ['b', () => cursor.prevWord()], // Alt+B
    ['d', () => cursor.deleteWordAfter()], // Alt+D
  ])

  // Claude Code's main input handler pattern
  function onInput(input: string, key: Key): void {
    logger.trace(
      `onInput: input="${input}" key=${JSON.stringify({ tab: key.tab, shift: key.shift, backspace: key.backspace, delete: key.delete })}`
    )

    // CRITICAL FIX: Block Shift+Tab completely at hook level
    if (key.tab && key.shift) {
      logger.trace('Blocking Shift+Tab completely - NO PROCESSING')
      return // Don't process anything for Shift+Tab
    }

    // Note: Shift+Tab and ESC are now blocked at the useInput level
    // This function should only receive events that need component-level processing

    // Handle backspace/delete first (Claude Code pattern)
    if (key.backspace || key.delete || input === '\b' || input === '\x7f' || input === '\x08') {
      const nextCursor = cursor.backspace()
      if (!cursor.equals(nextCursor)) {
        onCursorOffsetChange(nextCursor.offset)
        if (cursor.text !== nextCursor.text) {
          onChange(nextCursor.text)
        }
      }
      return
    }

    // Get next cursor from key mapping
    const nextCursor = mapKey(key)(input)
    if (nextCursor) {
      if (!cursor.equals(nextCursor)) {
        logger.trace(
          `Cursor change detected: ${cursor.offset} → ${nextCursor.offset} (key: ${JSON.stringify({ tab: key.tab, shift: key.shift })})`
        )
        onCursorOffsetChange(nextCursor.offset)
        if (cursor.text !== nextCursor.text) {
          logger.trace(`Text change detected: "${cursor.text}" → "${nextCursor.text}"`)
          onChange(nextCursor.text)
        }
      } else {
        logger.trace(
          `No cursor change: cursor.equals(nextCursor) = true (key: ${JSON.stringify({ tab: key.tab, shift: key.shift })})`
        )
      }
    }
  }

  // Claude Code's key mapping function
  function mapKey(key: Key): InputMapper {
    logger.trace(
      `mapKey: ${JSON.stringify({ tab: key.tab, shift: key.shift, upArrow: key.upArrow, downArrow: key.downArrow })}`
    )
    switch (true) {
      case key.leftArrow && (key.ctrl || key.meta):
        return () => cursor.prevWord()
      case key.rightArrow && (key.ctrl || key.meta):
        return () => cursor.nextWord()
      case key.ctrl:
        return handleCtrl
      case key.meta:
        return handleMeta
      case key.return:
        return () => {
          handleEnter()
          return cursor // Don't change cursor for Enter
        }
      case key.upArrow:
        return upOrHistoryUp
      case key.downArrow:
        return downOrHistoryDown
      case key.leftArrow:
        return () => cursor.left()
      case key.rightArrow:
        return () => cursor.right()
      case key.tab && !key.shift:
        return () => {
          logger.trace('Regular Tab detected in hybrid hook')
          // If command suggestions are visible, accept current suggestion
          if (hasCommandSuggestions) {
            onCommandSuggestionAccept?.()
            return cursor // Don't change cursor for tab acceptance
          }
          // Otherwise, insert tab character
          logger.trace('Inserting tab character')
          return cursor.insert('\t')
        }
      case key.tab && key.shift:
        return () => {
          logger.trace('Shift+Tab detected in hybrid hook - cursor should NOT advance')
          logger.trace(`Current cursor state: offset=${cursor.offset}, text="${cursor.text}"`)
          // Handle mode cycling via framework
          if (framework && typeof framework.setMode === 'function') {
            const currentMode = framework.getMode()
            const modes = ['interactive', 'command', 'streaming'] as const
            const currentIndex = modes.indexOf(currentMode)
            const nextMode = modes[(currentIndex + 1) % modes.length]
            logger.trace(`Mode cycling: ${currentMode} → ${nextMode}`)
            framework.setMode(nextMode)
          }
          logger.trace(`Returning unchanged cursor: offset=${cursor.offset}, text="${cursor.text}"`)
          return cursor // Don't change cursor for mode cycling
        }
    }

    // Handle regular character input (Claude Code pattern)
    return (input: string) => {
      switch (true) {
        // Home key
        case input === '\x1b[H' || input === '\x1b[1~':
          return cursor.startOfLine()
        // End key
        case input === '\x1b[F' || input === '\x1b[4~':
          return cursor.endOfLine()
        // Handle backspace character explicitly
        case input === '\b' || input === '\x7f' || input === '\x08':
          return cursor.backspace()
        // Note: Tab characters (\t) should only be inserted through explicit tab key handling above
        // If we reach this fallback with a tab character, it's likely from Shift+Tab spillover
        case input === '\t':
          logger.trace('Blocking stray tab character - likely from Shift+Tab spillover')
          return cursor // Don't insert tab characters that reach the fallback
        default:
          return cursor.insert(input)
      }
    }
  }

  // Use Claude Code's cursor.render() for proper display
  const renderedValue = cursor.render(' ', '', (text: string) => chalk.inverse(text))

  return {
    onInput,
    renderedValue,
  }
}
