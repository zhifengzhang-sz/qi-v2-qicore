/**
 * CLI Frameworks - Two framework implementations
 *
 * This module provides access to the two supported CLI frameworks:
 * - readline (custom, zero dependencies)
 * - ink (React-based rich UI)
 */

// Ink framework (complete implementation - requires: bun add ink @inkjs/ui)
export * from './ink/framework-index'
export { checkInkSupport } from './ink/framework-index'
// Readline framework (complete implementation)
export * from './readline/index'
// Framework support checking
export { checkReadlineSupport } from './readline/index'

import { checkInkSupport } from './ink/framework-index'
// Import functions for internal use
import { checkReadlineSupport } from './readline/index'

// Type definitions
export type CLIFrameworkType = 'readline' | 'ink'

/**
 * Get all available frameworks in the current environment
 */
export function getAvailableFrameworks(): CLIFrameworkType[] {
  const frameworks: CLIFrameworkType[] = ['readline'] // Always available

  if (checkInkSupport().available) {
    frameworks.push('ink')
  }

  return frameworks
}

/**
 * Get framework support information
 */
export function getAllFrameworkSupport(): Record<CLIFrameworkType, any> {
  return {
    readline: checkReadlineSupport(),
    ink: checkInkSupport(),
  }
}
