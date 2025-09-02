/**
 * Animated Loading Indicator Component
 *
 * Inspired by Claude Code's loading animations with rotating ASCII frames
 * and dynamic processing messages.
 */

import { useState, useEffect } from 'react'
import { Text } from 'ink'
import { createAnimatedLoader, defaultTheme, textStyles } from '../styles/theme'

interface LoadingIndicatorProps {
  message?: string
  showAnimation?: boolean
  color?: string
}

export function LoadingIndicator({
  message = 'Processing',
  showAnimation = true,
  color = textStyles.processing.color,
}: LoadingIndicatorProps) {
  const [frameIndex, setFrameIndex] = useState(0)
  const [messageIndex, setMessageIndex] = useState(0)

  useEffect(() => {
    if (!showAnimation) return

    const interval = setInterval(() => {
      setFrameIndex((prev) => {
        const newIndex = prev + 1
        // Change processing message less frequently than animation frames
        if (newIndex % 6 === 0) {
          setMessageIndex((prevMsg) => prevMsg + 1)
        }
        return newIndex
      })
    }, defaultTheme.animation.interval)

    return () => clearInterval(interval)
  }, [showAnimation])

  if (!showAnimation) {
    return <Text color={color}>{message}</Text>
  }

  // Use custom message or animated message
  const displayText =
    message === 'Processing'
      ? createAnimatedLoader(frameIndex, messageIndex)
      : `${defaultTheme.animation.loadingFrames[frameIndex % defaultTheme.animation.loadingFrames.length]} ${message}...`

  return <Text color={color}>{displayText}</Text>
}

/**
 * Simple loading spinner without text
 */
export function LoadingSpinner({ color = textStyles.processing.color }: { color?: string }) {
  const [frameIndex, setFrameIndex] = useState(0)

  useEffect(() => {
    const interval = setInterval(() => {
      setFrameIndex((prev) => prev + 1)
    }, defaultTheme.animation.interval)

    return () => clearInterval(interval)
  }, [])

  return (
    <Text color={color}>
      {
        defaultTheme.animation.loadingFrames[
          frameIndex % defaultTheme.animation.loadingFrames.length
        ]
      }
    </Text>
  )
}
