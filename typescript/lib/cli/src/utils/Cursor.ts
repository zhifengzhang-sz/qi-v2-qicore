/**
 * Cursor utility class ported from Claude Code
 *
 * Provides sophisticated cursor navigation for multiline text input
 * with proper line wrapping and position calculations.
 */

import { MeasuredText } from './MeasuredText'

type Position = {
  line: number
  column: number
}

export class Cursor {
  readonly offset: number

  constructor(
    readonly measuredText: MeasuredText,
    offset: number = 0,
    readonly selection: number = 0
  ) {
    // it's ok for the cursor to be 1 char beyond the end of the string
    this.offset = Math.max(0, Math.min(this.measuredText.text.length, offset))
  }

  static fromText(
    text: string,
    columns: number,
    offset: number = 0,
    selection: number = 0
  ): Cursor {
    // make MeasuredText on less than columns width, to account for cursor
    return new Cursor(new MeasuredText(text, columns - 1), offset, selection)
  }

  render(cursorChar: string, mask: string, invert: (text: string) => string) {
    const { line, column } = this.getPosition()
    return this.measuredText
      .getWrappedText()
      .map((text, currentLine, allLines) => {
        let displayText = text
        if (mask && currentLine === allLines.length - 1) {
          const lastSixStart = Math.max(0, text.length - 6)
          displayText = mask.repeat(lastSixStart) + text.slice(lastSixStart)
        }
        // looking for the line with the cursor
        if (line !== currentLine) return displayText.trimEnd()

        return (
          displayText.slice(0, column) +
          invert(displayText[column] || cursorChar) +
          displayText.trimEnd().slice(column + 1)
        )
      })
      .join('\n')
  }

  left(): Cursor {
    return new Cursor(this.measuredText, this.offset - 1)
  }

  right(): Cursor {
    return new Cursor(this.measuredText, this.offset + 1)
  }

  up(): Cursor {
    const { line, column } = this.getPosition()
    if (line === 0) {
      return new Cursor(this.measuredText, 0, 0)
    }

    const newOffset = this.getOffset({ line: line - 1, column })
    return new Cursor(this.measuredText, newOffset, 0)
  }

  down(): Cursor {
    const { line, column } = this.getPosition()
    if (line >= this.measuredText.lineCount - 1) {
      // Claude Code behavior: move cursor to end of text when can't move down
      return new Cursor(this.measuredText, this.text.length, 0)
    }

    const newOffset = this.getOffset({ line: line + 1, column })
    return new Cursor(this.measuredText, newOffset, 0)
  }

  startOfLine(): Cursor {
    const { line } = this.getPosition()
    return new Cursor(
      this.measuredText,
      this.getOffset({
        line,
        column: 0,
      }),
      0
    )
  }

  endOfLine(): Cursor {
    const { line } = this.getPosition()
    const column = this.measuredText.getLineLength(line)
    const offset = this.getOffset({ line, column })
    return new Cursor(this.measuredText, offset, 0)
  }

  nextWord(): Cursor {
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    let nextCursor: Cursor = this
    // If we're on a word, move to the next non-word
    while (nextCursor.isOverWordChar() && !nextCursor.isAtEnd()) {
      nextCursor = nextCursor.right()
    }
    // now move to the next word char
    while (!nextCursor.isOverWordChar() && !nextCursor.isAtEnd()) {
      nextCursor = nextCursor.right()
    }
    return nextCursor
  }

  prevWord(): Cursor {
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    let cursor: Cursor = this

    // if we are already at the beginning of a word, step off it
    if (!cursor.left().isOverWordChar()) {
      cursor = cursor.left()
    }

    // Move left over any non-word characters
    while (!cursor.isOverWordChar() && !cursor.isAtStart()) {
      cursor = cursor.left()
    }

    // If we're over a word character, move to the start of this word
    if (cursor.isOverWordChar()) {
      while (cursor.left().isOverWordChar() && !cursor.isAtStart()) {
        cursor = cursor.left()
      }
    }

    return cursor
  }

  private modifyText(end: Cursor, insertString: string = ''): Cursor {
    const startOffset = this.offset
    const endOffset = end.offset

    const newText = this.text.slice(0, startOffset) + insertString + this.text.slice(endOffset)

    return Cursor.fromText(newText, this.columns, startOffset + insertString.length)
  }

  insert(insertString: string): Cursor {
    const newCursor = this.modifyText(this, insertString)
    return newCursor
  }

  del(): Cursor {
    if (this.isAtEnd()) {
      return this
    }
    return this.modifyText(this.right())
  }

  backspace(): Cursor {
    if (this.isAtStart()) {
      return this
    }

    // Get the current position
    const currentOffset = this.offset

    // Create a new cursor at the position before the current one
    const leftCursor = this.left()
    const leftOffset = leftCursor.offset

    // Create the new text by removing one character
    const newText = this.text.slice(0, leftOffset) + this.text.slice(currentOffset)

    // Return a new cursor with the updated text and position
    return Cursor.fromText(newText, this.columns, leftOffset)
  }

  deleteToLineStart(): Cursor {
    return this.startOfLine().modifyText(this)
  }

  deleteToLineEnd(): Cursor {
    // If cursor is on a newline character, delete just that character
    if (this.text[this.offset] === '\n') {
      return this.modifyText(this.right())
    }

    return this.modifyText(this.endOfLine())
  }

  deleteWordBefore(): Cursor {
    if (this.isAtStart()) {
      return this
    }
    return this.prevWord().modifyText(this)
  }

  deleteWordAfter(): Cursor {
    if (this.isAtEnd()) {
      return this
    }

    return this.modifyText(this.nextWord())
  }

  private isOverWordChar(): boolean {
    const currentChar = this.text[this.offset] ?? ''
    return /\w/.test(currentChar)
  }

  equals(other: Cursor): boolean {
    return this.offset === other.offset && this.measuredText === other.measuredText
  }

  private isAtStart(): boolean {
    return this.offset === 0
  }

  private isAtEnd(): boolean {
    return this.offset === this.text.length
  }

  public get text(): string {
    return this.measuredText.text
  }

  private get columns(): number {
    return this.measuredText.columns + 1
  }

  private getPosition(): Position {
    return this.measuredText.getPositionFromOffset(this.offset)
  }

  private getOffset(position: Position): number {
    return this.measuredText.getOffsetFromPosition(position)
  }
}
