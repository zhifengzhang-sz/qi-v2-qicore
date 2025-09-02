/**
 * MeasuredText class ported from Claude Code
 *
 * Handles text measurement, wrapping, and position calculations
 * for sophisticated multiline cursor navigation.
 */

type WrappedText = string[]
type Position = {
  line: number
  column: number
}

class WrappedLine {
  constructor(
    public readonly text: string,
    public readonly startOffset: number,
    public readonly isPrecededByNewline: boolean,
    public readonly endsWithNewline: boolean = false
  ) {}

  equals(other: WrappedLine): boolean {
    return this.text === other.text && this.startOffset === other.startOffset
  }

  get length(): number {
    return this.text.length + (this.endsWithNewline ? 1 : 0)
  }
}

export class MeasuredText {
  private wrappedLines: WrappedLine[]

  constructor(
    readonly text: string,
    readonly columns: number
  ) {
    this.wrappedLines = this.measureWrappedText()
  }

  private measureWrappedText(): WrappedLine[] {
    // Simple text wrapping - split by columns width
    // For a more sophisticated implementation, we could use a library like wrap-ansi
    const wrappedText = this.wrapText(this.text, this.columns)

    const wrappedLines: WrappedLine[] = []
    let searchOffset = 0
    let lastNewLinePos = -1

    const lines = wrappedText.split('\n')
    for (let i = 0; i < lines.length; i++) {
      const text = lines[i]!
      const isPrecededByNewline = (startOffset: number) =>
        i === 0 || (startOffset > 0 && this.text[startOffset - 1] === '\n')

      if (text.length === 0) {
        // For blank lines, find the next newline character after the last one
        lastNewLinePos = this.text.indexOf('\n', lastNewLinePos + 1)

        if (lastNewLinePos !== -1) {
          const startOffset = lastNewLinePos
          const endsWithNewline = true

          wrappedLines.push(
            new WrappedLine(text, startOffset, isPrecededByNewline(startOffset), endsWithNewline)
          )
        } else {
          // If we can't find another newline, this must be the end of text
          const startOffset = this.text.length
          wrappedLines.push(
            new WrappedLine(text, startOffset, isPrecededByNewline(startOffset), false)
          )
        }
      } else {
        // For non-blank lines
        const startOffset = this.text.indexOf(text, searchOffset)
        if (startOffset === -1) {
          // Fallback: use searchOffset as startOffset
          const fallbackStartOffset = Math.min(searchOffset, this.text.length)
          wrappedLines.push(
            new WrappedLine(
              text,
              fallbackStartOffset,
              isPrecededByNewline(fallbackStartOffset),
              false
            )
          )
          continue
        }

        searchOffset = startOffset + text.length

        // Check if this line ends with a newline in the original text
        const potentialNewlinePos = startOffset + text.length
        const endsWithNewline =
          potentialNewlinePos < this.text.length && this.text[potentialNewlinePos] === '\n'

        if (endsWithNewline) {
          lastNewLinePos = potentialNewlinePos
        }

        wrappedLines.push(
          new WrappedLine(text, startOffset, isPrecededByNewline(startOffset), endsWithNewline)
        )
      }
    }

    return wrappedLines
  }

  private wrapText(text: string, width: number): string {
    if (width <= 0) return text

    const lines = text.split('\n')
    const wrappedLines: string[] = []

    for (const line of lines) {
      if (line.length <= width) {
        wrappedLines.push(line)
      } else {
        // Split long lines at word boundaries when possible
        const words = line.split(' ')
        let currentLine = ''

        for (const word of words) {
          if (word.length > width) {
            // Word is longer than width, force break
            if (currentLine) {
              wrappedLines.push(currentLine.trim())
              currentLine = ''
            }
            // Break the long word
            for (let i = 0; i < word.length; i += width) {
              wrappedLines.push(word.slice(i, i + width))
            }
          } else if (`${currentLine} ${word}`.length <= width) {
            // Word fits on current line
            currentLine = currentLine ? `${currentLine} ${word}` : word
          } else {
            // Word doesn't fit, start new line
            if (currentLine) {
              wrappedLines.push(currentLine.trim())
            }
            currentLine = word
          }
        }

        if (currentLine) {
          wrappedLines.push(currentLine.trim())
        }
      }
    }

    return wrappedLines.join('\n')
  }

  public getWrappedText(): WrappedText {
    return this.wrappedLines.map((line) =>
      line.isPrecededByNewline ? line.text : line.text.trimStart()
    )
  }

  private getLine(line: number): WrappedLine {
    return this.wrappedLines[Math.max(0, Math.min(line, this.wrappedLines.length - 1))]!
  }

  public getOffsetFromPosition(position: Position): number {
    const wrappedLine = this.getLine(position.line)
    const startOffsetPlusColumn = wrappedLine.startOffset + position.column

    // Handle blank lines specially
    if (wrappedLine.text.length === 0 && wrappedLine.endsWithNewline) {
      return wrappedLine.startOffset
    }

    // For normal lines
    const lineEnd = wrappedLine.startOffset + wrappedLine.text.length
    // Add 1 only if this line ends with a newline
    const maxOffset = wrappedLine.endsWithNewline ? lineEnd + 1 : lineEnd

    return Math.min(startOffsetPlusColumn, maxOffset)
  }

  public getLineLength(line: number): number {
    const currentLine = this.getLine(line)
    const nextLine = this.getLine(line + 1)
    if (nextLine.equals(currentLine)) {
      return this.text.length - currentLine.startOffset
    }

    return Math.max(0, nextLine.startOffset - currentLine.startOffset - 1)
  }

  public getPositionFromOffset(offset: number): Position {
    const lines = this.wrappedLines
    for (let line = 0; line < lines.length; line++) {
      const currentLine = lines[line]!
      const nextLine = lines[line + 1]
      if (offset >= currentLine.startOffset && (!nextLine || offset < nextLine.startOffset)) {
        const leadingWhitespace = currentLine.isPrecededByNewline
          ? 0
          : currentLine.text.length - currentLine.text.trimStart().length
        const column = Math.max(
          0,
          Math.min(offset - currentLine.startOffset - leadingWhitespace, currentLine.text.length)
        )
        return {
          line,
          column,
        }
      }
    }

    // If we're past the last character, return the end of the last line
    const line = lines.length - 1
    return {
      line,
      column: this.wrappedLines[line]?.text.length || 0,
    }
  }

  public get lineCount(): number {
    return this.wrappedLines.length
  }

  equals(other: MeasuredText): boolean {
    return this.text === other.text && this.columns === other.columns
  }
}
