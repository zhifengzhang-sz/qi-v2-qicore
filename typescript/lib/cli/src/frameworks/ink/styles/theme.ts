/**
 * Ink CLI Theme and Styling System
 *
 * Centralized styling configuration for the entire CLI interface
 */

export interface CLITheme {
  colors: {
    primary: string
    secondary: string
    accent: string
    success: string
    warning: string
    error: string
    info: string
    dim: string
    text: string
    background: string
  }
  gradients: {
    header: string[]
    provider: string[]
    model: string[]
    rainbow: string
  }
  borders: {
    input: {
      style: 'single' | 'double' | 'round' | 'bold' | 'singleDouble' | 'doubleSingle' | 'classic'
      colorActive: string
      colorBusy: string
    }
  }
  spacing: {
    none: number
    xs: number
    sm: number
    md: number
    lg: number
    xl: number
  }
  layout: {
    headerAlignment: 'flex-start' | 'center' | 'flex-end' | 'space-between' | 'space-around'
    statusAlignment: 'flex-start' | 'center' | 'flex-end' | 'space-between' | 'space-around'
  }
  animation: {
    loadingFrames: string[]
    processingMessages: string[]
    interval: number
  }
}

export const defaultTheme: CLITheme = {
  colors: {
    primary: '#007acc', // Claude Code blue
    secondary: '#ff6b35', // Claude Code orange accent
    accent: '#4caf50', // Claude Code green
    success: '#4caf50', // Green for success states
    warning: '#ff9800', // Orange for warnings
    error: '#f44336', // Red for errors
    info: '#2196f3', // Blue for info
    dim: 'dim', // Dimmed text
    text: '#e0e0e0', // Light gray text (better than white)
    background: '#0d1117', // Dark background like GitHub/Claude Code
  },
  gradients: {
    header: ['#007acc', '#0099ff', '#00bfff'], // Blue gradient
    provider: ['#ff6b35', '#ff8c42'], // Orange gradient
    model: ['#4caf50', '#66bb6a'], // Green gradient
    rainbow: 'rainbow',
  },
  borders: {
    input: {
      style: 'round', // Softer rounded borders like Claude Code
      colorActive: '#4caf50',
      colorBusy: '#ff9800',
    },
  },
  spacing: {
    none: 0,
    xs: 1,
    sm: 2,
    md: 4,
    lg: 8,
    xl: 16,
  },
  layout: {
    headerAlignment: 'flex-start',
    statusAlignment: 'flex-start',
  },
  animation: {
    loadingFrames: ['·', '✢', '✳', '∗', '✻', '✽'],
    processingMessages: ['Processing', 'Thinking', 'Responding', 'Loading'],
    interval: 120, // ms between frame changes
  },
}

export const styles = {
  header: {
    padding: defaultTheme.spacing.xs,
    justifyContent: defaultTheme.layout.headerAlignment,
  },
  content: {
    flexGrow: 1,
    flexDirection: 'column' as const,
  },
  inputContainer: {
    borderStyle: defaultTheme.borders.input.style,
    paddingX: defaultTheme.spacing.sm,
    paddingY: defaultTheme.spacing.none, // Already at zero
    minHeight: 1, // Force minimum height
  },
  statusLine: {
    paddingX: defaultTheme.spacing.sm,
    paddingY: defaultTheme.spacing.none, // Zero gap!
    justifyContent: 'space-between' as const,
    alignItems: 'center' as const,
  },
  statusLeft: {
    flexDirection: 'row' as const,
    alignItems: 'center' as const,
  },
  statusRight: {
    flexDirection: 'row' as const,
    alignItems: 'center' as const,
  },
} as const

export const getInputBorderColor = (state: 'ready' | 'busy') => {
  return state === 'busy'
    ? defaultTheme.borders.input.colorBusy
    : defaultTheme.borders.input.colorActive
}

export const textStyles = {
  header: {
    color: defaultTheme.colors.text,
    bold: true,
  },
  provider: {
    color: defaultTheme.colors.text,
    bold: true,
  },
  model: {
    color: defaultTheme.colors.text,
    bold: true,
  },
  mode: {
    color: defaultTheme.colors.accent,
    bold: false,
  },
  separator: {
    color: defaultTheme.colors.dim,
  },
  processing: {
    color: defaultTheme.colors.warning,
  },
  progress: {
    color: defaultTheme.colors.info,
  },
  progressBar: {
    complete: '█',
    incomplete: '░',
    width: 40,
    showPercentage: true,
  },
} as const

export const createProgressBar = (
  progress: number,
  width: number = textStyles.progressBar.width
) => {
  const filled = Math.round(progress * width)
  const empty = width - filled
  const bar =
    textStyles.progressBar.complete.repeat(filled) + textStyles.progressBar.incomplete.repeat(empty)
  const percentage = Math.round(progress * 100)
  return `[${bar}] ${percentage}%`
}

/**
 * Animation utilities inspired by Claude Code's loading patterns
 */
export const getLoadingFrame = (frameIndex: number): string => {
  const frames = defaultTheme.animation.loadingFrames
  const frame = frames[frameIndex % frames.length]
  return frame || '⠋'
}

export const getProcessingMessage = (messageIndex: number): string => {
  const messages = defaultTheme.animation.processingMessages
  const message = messages[messageIndex % messages.length]
  return message || 'Processing...'
}

export const createAnimatedLoader = (frameIndex: number, messageIndex: number): string => {
  const frame = getLoadingFrame(frameIndex)
  const message = getProcessingMessage(messageIndex)
  return `${frame} ${message}...`
}
