/**
 * Readline Framework - Zero dependency CLI implementation
 *
 * This module exports all readline framework components that implement
 * the CLI abstractions using only Node.js built-ins and ANSI escape sequences.
 */

export type {
  IInputManager,
  InputConfig,
  KeypressData,
} from '../../abstractions/IInputManager'
// Re-export interfaces for convenience
export type {
  ITerminal,
  TerminalDimensions,
} from '../../abstractions/ITerminal'
export type {
  IModeRenderer,
  IProgressRenderer,
  IStreamRenderer,
  ProgressConfig,
} from '../../abstractions/IUIComponent'
export { ReadlineInputManager } from './ReadlineInputManager'
export { ReadlineModeRenderer } from './ReadlineModeRenderer'
export { ReadlineProgressRenderer } from './ReadlineProgressRenderer'
export { ReadlineStreamRenderer } from './ReadlineStreamRenderer'
// Core readline framework implementations
export { ReadlineTerminal } from './ReadlineTerminal'

/**
 * Readline framework availability check
 * Always available as it uses only Node.js built-ins
 */
export function checkReadlineSupport(): { available: boolean; reason: string } {
  return {
    available: true,
    reason: 'Readline framework uses only Node.js built-ins (always available)',
  }
}
