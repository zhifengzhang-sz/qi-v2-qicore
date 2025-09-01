/**
 * Ink Framework (STUB)
 *
 * React-based rich UI framework implementation stubs.
 * These will be expanded when Ink framework support is fully implemented.
 *
 * Dependencies: bun add ink @inkjs/ui ink-progress-bar ink-spinner
 */

// Stub implementations
// InkTerminal removed - was fake stub with fallbacks

// TODO: Implement remaining components when Ink support is added
// export { InkInputManager } from './InkInputManager';
// export { InkProgressRenderer } from './InkProgressRenderer';
// export { InkModeRenderer } from './InkModeRenderer';
// export { InkStreamRenderer } from './InkStreamRenderer';

// Re-export interfaces for convenience
export type {
  ITerminal,
  TerminalDimensions,
} from '../../abstractions/ITerminal'

/**
 * Ink framework availability check
 */
export function checkInkSupport(): { available: boolean; reason: string; packages?: string[] } {
  // For bundling compatibility, assume availability in development
  const isDevelopment = process.env.NODE_ENV !== 'production'

  if (isDevelopment) {
    return {
      available: true,
      reason: 'Ink support assumed in development environment',
    }
  }

  // In production/bundled environment, defer to runtime check
  return {
    available: false,
    reason: 'Ink availability determined at runtime',
    packages: ['ink', '@inkjs/ui', 'ink-progress-bar', 'ink-spinner'],
  }
}
