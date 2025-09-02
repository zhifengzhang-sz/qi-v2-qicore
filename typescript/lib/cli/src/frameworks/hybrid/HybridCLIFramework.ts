/**
 * Hybrid CLI Framework Implementation - CORRECT APPROACH
 *
 * Based on Claude Code analysis - uses Ink's React components for UI
 * with enhanced cursor navigation logic (no raw terminal takeover).
 *
 * Architecture:
 * - Extends InkCLIFramework for full React/Ink UI capabilities
 * - Adds sophisticated Cursor-based navigation logic
 * - Claude Code-style down arrow behavior: move to end → history
 * - QiCore integration with proper logging and error handling
 */

import { create, failure, type QiError, type Result, success } from '@qi/base'
import type { QiAsyncMessageQueue } from '@qi/amsg'
import type { QiMessage } from '@qi/amsg'
import { Cursor } from '../../utils/Cursor'
import type { CLIConfig } from '../../abstractions/ICLIFramework'
import { InkCLIFramework } from '../ink/InkCLIFramework'

/**
 * QiCore error types for hybrid framework operations
 */
interface HybridFrameworkError extends QiError {
  context: {
    operation?: string
    cursorOffset?: number
    textLength?: number
    framework?: string
  }
}

const hybridError = (
  code: string,
  message: string,
  context: HybridFrameworkError['context'] = {}
): HybridFrameworkError =>
  create(code, message, 'SYSTEM', { framework: 'hybrid', ...context }) as HybridFrameworkError

/**
 * Hybrid CLI Framework that enhances Ink with Claude Code-style navigation
 *
 * Key Features:
 * - Full Ink React components for rich UI (progress bars, styling, etc.)
 * - Enhanced cursor navigation using Claude Code's Cursor class
 * - Smart down arrow behavior: cursor to end, then history navigation
 * - QiCore integration with structured logging and error handling
 * - Professional Result<T> patterns throughout
 */
export class HybridCLIFramework extends InkCLIFramework {
  public isHybridEnabled = true // Public flag for InputBox detection
  private cursorAtEndState = false // Track if cursor moved to end on last down arrow
  private rawStdinHandler?: (chunk: Buffer) => void
  private isRawModeActive = false

  constructor(config: Partial<CLIConfig> = {}, messageQueue?: QiAsyncMessageQueue<QiMessage>) {
    super(config, messageQueue)
    this.logger.debug('HybridCLIFramework constructor called - THIS IS HYBRID MODE')
    this.logger.debug('HybridCLIFramework calling initializeInfrastructure')
    this.initializeInfrastructure(config)
    this.logger.debug('HybridCLIFramework fully initialized')
  }

  /**
   * Initialize QiCore infrastructure
   * Note: Logger is already initialized by parent InkCLIFramework
   */
  private initializeInfrastructure(_config: Partial<CLIConfig>): void {
    // Basic hybrid framework initialization
    this.logger.debug('Hybrid CLI Framework initialized', {
      isHybridEnabled: this.isHybridEnabled,
      framework: 'hybrid',
      parent: 'ink',
    })

    // Set up raw stdin handling for special keys like Shift+Tab
    this.setupRawStdinHandling()
  }

  /**
   * Set up raw stdin handling for special keys (Claude Code approach)
   * This complements Ink's input handling for keys that Ink doesn't handle well
   */
  private setupRawStdinHandling(): void {
    // Only set up raw stdin handling in interactive TTY environments
    if (!process.stdin.isTTY) {
      this.logger.debug('Hybrid: Non-TTY environment detected, using fallback input mode')
      return
    }

    this.rawStdinHandler = (chunk: Buffer) => {
      const input = chunk.toString('utf8')
      const bytes = Array.from(chunk)

      this.logger.debug(
        `Hybrid raw stdin: Got [${bytes.map((b) => `0x${b.toString(16)}`).join(' ')}] "${input}"`
      )

      // Handle Shift+Tab (ESC[Z - 0x1b, 0x5b, 0x5a)
      if (bytes.length === 3 && bytes[0] === 0x1b && bytes[1] === 0x5b && bytes[2] === 0x5a) {
        this.logger.debug('Hybrid: Detected Shift+Tab - cycling mode')
        this.handleModeToggle()
        return
      }
    }

    this.logger.debug('Hybrid: Raw stdin handler configured for TTY environment')
  }

  /**
   * Handle mode cycling for Shift+Tab
   */
  private handleModeToggle(): void {
    const currentMode = this.getMode()
    const modes = ['interactive', 'command', 'streaming'] as const
    const currentIndex = modes.indexOf(currentMode)
    const nextMode = modes[(currentIndex + 1) % modes.length]

    if (nextMode) {
      this.logger.debug(`Hybrid: Mode change ${currentMode} → ${nextMode}`)
      this.setMode(nextMode)
    }
  }

  /**
   * Override start to enable raw stdin handling after Ink is set up
   */
  async start(): Promise<void> {
    // Call parent start first to set up Ink
    await super.start()

    // Now enable our raw stdin handling for special keys
    this.enableRawStdinHandling()
  }

  /**
   * Override shutdown to clean up raw stdin handling
   */
  async shutdown(): Promise<void> {
    this.disableRawStdinHandling()

    // Call parent shutdown
    await super.shutdown()
  }

  /**
   * Enable raw stdin handling for special keys
   */
  private enableRawStdinHandling(): void {
    if (this.isRawModeActive || !this.rawStdinHandler) {
      return
    }

    // Only enable raw mode if we're in an interactive TTY environment
    // This prevents conflicts when input is piped (like in tests)
    if (!process.stdin.isTTY) {
      this.logger.debug('⚠️ No TTY detected, using fallback mode')
      return
    }

    this.logger.debug('✅ Hybrid terminal input initialized')

    if (process.stdin.setRawMode) {
      try {
        process.stdin.setRawMode(true)
        process.stdin.resume()
        process.stdin.on('data', this.rawStdinHandler)
        this.isRawModeActive = true
        this.logger.debug('Hybrid: Raw stdin handling enabled')
      } catch (error) {
        this.logger.debug('Hybrid: Failed to enable raw mode:', error)
      }
    } else {
      this.logger.debug('Hybrid: Raw mode not supported')
    }
  }

  /**
   * Disable raw stdin handling
   */
  private disableRawStdinHandling(): void {
    if (!this.isRawModeActive || !this.rawStdinHandler) {
      return
    }

    this.logger.debug('Hybrid: Disabling raw stdin handling')

    try {
      process.stdin.removeListener('data', this.rawStdinHandler)
      if (process.stdin.setRawMode) {
        process.stdin.setRawMode(false)
      }
      this.isRawModeActive = false
      this.logger.debug('Hybrid: Raw stdin handling disabled')
    } catch (error) {
      this.logger.debug('Hybrid: Failed to disable raw mode:', error)
    }
  }

  /**
   * Enhanced input handling with Claude Code navigation logic
   * Uses proper QiCore Result<T> patterns - no try/catch
   */
  public handleClaudeCodeNavigation(
    text: string,
    cursorOffset: number,
    columns: number,
    key: { name?: string }
  ): Result<
    {
      shouldTriggerHistory: boolean
      newCursorOffset?: number
      resetCursorEndState?: boolean
    },
    HybridFrameworkError
  > {
    this.logger.info('Handling navigation input', {
      keyName: key.name,
      cursorOffset,
      textLength: text.length,
      columns,
    })

    if (key.name === 'down') {
      return this.handleDownArrowNavigation(text, cursorOffset, columns)
    } else if (key.name === 'up') {
      // Reset cursor end state on up arrow
      this.cursorAtEndState = false
      this.logger.debug('Reset cursor state on up arrow', {
        cursorAtEndState: this.cursorAtEndState,
      })
      return success({ shouldTriggerHistory: false, resetCursorEndState: true })
    } else {
      // Reset cursor end state on any other key
      this.cursorAtEndState = false
      this.logger.debug('Reset cursor state on key press', {
        keyName: key.name,
        cursorAtEndState: this.cursorAtEndState,
      })
      return success({ shouldTriggerHistory: false, resetCursorEndState: true })
    }
  }

  /**
   * Handle down arrow with Claude Code two-step behavior
   * Pure QiCore functional approach - no try/catch needed
   */
  private handleDownArrowNavigation(
    text: string,
    cursorOffset: number,
    columns: number
  ): Result<
    {
      shouldTriggerHistory: boolean
      newCursorOffset?: number
    },
    HybridFrameworkError
  > {
    // Validate inputs using QiCore validation patterns
    if (cursorOffset < 0 || cursorOffset > text.length) {
      const error = hybridError('INVALID_CURSOR_OFFSET', 'Cursor offset is out of bounds', {
        operation: 'handleDownArrowNavigation',
        cursorOffset,
        textLength: text.length,
      })
      this.logger.warn('Invalid cursor offset detected', {
        cursorOffset,
        textLength: text.length,
        error: error.message,
      })
      return failure(error)
    }

    if (columns <= 0) {
      const error = hybridError('INVALID_COLUMNS', 'Column width must be positive', {
        operation: 'handleDownArrowNavigation',
        cursorOffset,
        textLength: text.length,
      })
      this.logger.warn('Invalid column width', {
        columns,
        error: error.message,
      })
      return failure(error)
    }

    // Create cursor from current state
    const cursor = Cursor.fromText(text, columns, cursorOffset)

    // Try to move cursor down
    const cursorDown = cursor.down()

    // Check if cursor movement succeeded (Claude Code logic)
    if (cursorDown.equals(cursor)) {
      // Cursor can't move down - we're on the last line
      this.logger.debug('Cursor on last line, applying Claude Code behavior', {
        cursorAtEndState: this.cursorAtEndState,
        cursorOffset,
        textLength: text.length,
      })

      if (!this.cursorAtEndState && cursorOffset < text.length) {
        // STEP 1: Move cursor to end of input
        this.cursorAtEndState = true
        this.logger.info('Step 1: Moving cursor to end of input', {
          oldOffset: cursorOffset,
          newOffset: text.length,
        })
        return success({
          shouldTriggerHistory: false,
          newCursorOffset: text.length,
        })
      } else {
        // STEP 2: Cursor already at end - trigger history navigation
        this.cursorAtEndState = false
        this.logger.info('Step 2: Triggering history navigation', {
          cursorOffset,
          textLength: text.length,
        })
        return success({ shouldTriggerHistory: true })
      }
    } else {
      // Cursor moved successfully to next line - normal navigation
      this.cursorAtEndState = false
      this.logger.debug('Normal cursor down movement', {
        oldOffset: cursorOffset,
        newOffset: cursorDown.offset,
      })
      return success({
        shouldTriggerHistory: false,
        newCursorOffset: cursorDown.offset,
      })
    }
  }

  /**
   * Create Cursor instance for external use (by InputBox)
   * Pure QiCore functional approach
   */
  public createCursor(
    text: string,
    columns: number,
    offset: number = 0
  ): Result<Cursor, HybridFrameworkError> {
    // Validation using QiCore patterns
    if (columns <= 0) {
      return failure(
        hybridError('INVALID_COLUMNS', 'Column width must be positive', {
          operation: 'createCursor',
        })
      )
    }

    if (offset < 0 || offset > text.length) {
      return failure(
        hybridError('INVALID_CURSOR_OFFSET', 'Cursor offset is out of bounds', {
          operation: 'createCursor',
          cursorOffset: offset,
          textLength: text.length,
        })
      )
    }

    // QiCore: operations that can succeed, we just do directly
    const cursor = Cursor.fromText(text, columns, offset)

    this.logger.debug('Cursor created successfully', {
      textLength: text.length,
      columns,
      offset: cursor.offset,
    })

    return success(cursor)
  }

  /**
   * Get cursor navigation methods for InputBox integration
   */
  public getCursorMethods(cursor: Cursor) {
    return {
      up: () => cursor.up(),
      down: () => cursor.down(),
      left: () => cursor.left(),
      right: () => cursor.right(),
      startOfLine: () => cursor.startOfLine(),
      endOfLine: () => cursor.endOfLine(),
      nextWord: () => cursor.nextWord(),
      prevWord: () => cursor.prevWord(),
      insert: (text: string) => cursor.insert(text),
      backspace: () => cursor.backspace(),
      del: () => cursor.del(),
    }
  }

  /**
   * Check if two cursors are at the same position (for Claude Code logic)
   */
  public cursorsEqual(cursor1: Cursor, cursor2: Cursor): boolean {
    return cursor1.equals(cursor2)
  }

  /**
   * Reset cursor end state (called by InputBox on input changes)
   */
  public resetCursorEndState(): void {
    this.cursorAtEndState = false
  }

  /**
   * Override parent's input handling to add hybrid navigation
   */
  public handleInput(input: string): void {
    // Reset cursor state on new input
    this.resetCursorEndState()

    // Delegate to parent Ink framework
    super.handleInput(input)
  }

  /**
   * Get hybrid framework statistics
   */
  public getHybridStats() {
    return {
      isHybridEnabled: this.isHybridEnabled,
      cursorAtEndState: this.cursorAtEndState,
      framework: 'hybrid',
      parentFramework: 'ink',
    }
  }
}
