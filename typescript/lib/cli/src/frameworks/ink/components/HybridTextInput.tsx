import { Text, useInput } from 'ink'
import { createDebugLogger } from '../../../utils'
import { useHybridTextInput } from '../../hybrid/hooks/useHybridTextInput'
import type { HybridCLIFramework } from '../../hybrid/HybridCLIFramework'

export interface HybridTextInputProps {
  /**
   * Current text value
   */
  value: string

  /**
   * Called when text changes
   */
  onChange: (value: string) => void

  /**
   * Called when Enter is pressed
   */
  onSubmit?: (value: string) => void

  /**
   * Called when up arrow triggers history navigation
   */
  onHistoryUp?: () => void

  /**
   * Called when down arrow triggers history navigation
   */
  onHistoryDown?: () => void

  /**
   * Called when up arrow is used to navigate command suggestions
   */
  onCommandSuggestionUp?: () => void

  /**
   * Called when down arrow is used to navigate command suggestions
   */
  onCommandSuggestionDown?: () => void

  /**
   * Called when Tab is pressed to accept current command suggestion
   */
  onCommandSuggestionAccept?: () => void

  /**
   * Whether command suggestions are currently visible
   */
  hasCommandSuggestions?: boolean

  /**
   * Placeholder text when empty
   */
  placeholder?: string

  /**
   * Whether component should listen to input
   */
  focus?: boolean

  /**
   * Terminal column width for text wrapping
   */
  columns?: number

  /**
   * Current cursor position
   */
  cursorOffset: number

  /**
   * Called when cursor position changes
   */
  onCursorOffsetChange: (offset: number) => void

  /**
   * Hybrid framework instance for enhanced navigation
   */
  framework?: HybridCLIFramework
}

/**
 * HybridTextInput Component
 *
 * Follows Claude Code's architecture:
 * - Minimal presentation component
 * - All logic delegated to hook
 * - Clean props interface
 * - Proper cursor rendering
 */
export function HybridTextInput({
  value,
  onChange,
  onSubmit,
  onHistoryUp,
  onHistoryDown,
  onCommandSuggestionUp,
  onCommandSuggestionDown,
  onCommandSuggestionAccept,
  hasCommandSuggestions = false,
  placeholder = '',
  focus = true,
  columns = 80,
  cursorOffset,
  onCursorOffsetChange,
  framework,
}: HybridTextInputProps) {
  const logger = createDebugLogger('HybridTextInput')
  logger.trace('HybridTextInput component rendered - this should show if hybrid mode is active')

  // Delegate all logic to hook (Claude Code pattern)
  const { onInput, renderedValue } = useHybridTextInput({
    value,
    onChange,
    onSubmit,
    onHistoryUp,
    onHistoryDown,
    onCommandSuggestionUp,
    onCommandSuggestionDown,
    onCommandSuggestionAccept,
    hasCommandSuggestions,
    columns,
    cursorOffset,
    onCursorOffsetChange,
    framework,
  })

  // Capture input events (Claude Code pattern)
  // CRITICAL FIX: Block global hotkeys to prevent dual processing and cursor corruption
  useInput(
    (input, key) => {
      logger.trace(
        `useInput: input="${input}" key=${JSON.stringify({ tab: key.tab, shift: key.shift, escape: key.escape })}`
      )

      // Block ESC for HotkeyManager, but let Shift+Tab through for hybrid handling
      if (key.escape) {
        logger.trace('Blocking ESC for HotkeyManager')
        // Don't process - these will be handled by HotkeyManager
        return
      }

      // CRITICAL FIX: Block Shift+Tab at Ink level to prevent tab character insertion
      if (key.tab && key.shift) {
        logger.trace('Blocking Shift+Tab at Ink level to prevent cursor advancement')
        // Let the hybrid hook handle mode cycling, but don't process the tab character
        onInput('', key) // Pass empty input to prevent tab insertion
        return
      }

      // Process all other input normally
      logger.trace(`Processing input normally: "${input}"`)
      onInput(input, key)
    },
    { isActive: focus }
  )

  // Show placeholder when empty (Claude Code pattern)
  if (value === '' && placeholder) {
    return <Text dimColor>{placeholder}</Text>
  }

  // Render using cursor.render() output (Claude Code pattern)
  return <Text>{renderedValue}</Text>
}
