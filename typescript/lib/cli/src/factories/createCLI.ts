/**
 * Framework-agnostic CLI Factory
 *
 * Provides a unified interface for creating CLI instances with different frameworks:
 * - readline (default, zero dependencies)
 * - ink (React-based rich UI)
 */

import { create, Err, match, Ok, type QiError, type Result } from '@qi/base'
import { createDebugLogger } from '../utils/DebugLogger'
import type { CLIConfig, ICLIFramework } from '../abstractions/ICLIFramework'
import { HybridCLIFramework } from '../frameworks/hybrid/HybridCLIFramework'
// TEMP: Static imports for development to avoid Zod conflicts
// TODO: Remove these for binary compilation
import { InkCLIFramework } from '../frameworks/ink/InkCLIFramework'
// NOTE: Ink framework is imported dynamically to avoid top-level await issues in binary compilation
// Framework factories
import {
  checkReadlineSupport,
  createReadlineCLI,
  createReadlineCLIAsync,
  createValidatedReadlineCLI,
} from './createReadlineCLI'

/**
 * Framework types supported by the CLI
 */
export type CLIFramework = 'readline' | 'ink' | 'hybrid'

/**
 * Extended CLI configuration with framework selection
 */
export interface CLIConfigWithFramework extends CLIConfig {
  framework?: CLIFramework
}

/**
 * Factory error types
 */
interface CLIFactoryError extends QiError {
  context: {
    framework?: CLIFramework
    operation?: string
    availableFrameworks?: CLIFramework[]
    supportCheck?: any
  }
}

const cliFactoryError = (
  code: string,
  message: string,
  context: CLIFactoryError['context'] = {}
): CLIFactoryError => create(code, message, 'SYSTEM', context) as CLIFactoryError

/**
 * Create a CLI instance with the specified framework
 *
 * @param config - CLI configuration including framework selection
 * @returns Result containing the CLI instance or error
 */
export function createCLI(
  config: Partial<CLIConfigWithFramework> = {}
): Result<ICLIFramework, QiError> {
  const logger = createDebugLogger('CLIFactory')
  logger.trace('createCLI - received config keys:', Object.keys(config))
  logger.trace('createCLI - stateManager:', !!config.stateManager)
  logger.trace('createCLI - messageQueue:', !!config.messageQueue)

  const framework = config.framework || 'readline'
  logger.trace(`createCLI - using framework: ${framework} (config.framework: ${config.framework})`)

  switch (framework) {
    case 'readline':
      return createReadlineCLI(config)

    case 'ink':
      return createInkCLI(config)

    case 'hybrid':
      return Err(
        cliFactoryError(
          'HYBRID_NOT_SUPPORTED_SYNC',
          'Hybrid framework not supported in sync createCLI. Use createCLIAsync() instead.',
          {
            framework: 'hybrid',
            operation: 'createCLI',
          }
        )
      )

    default:
      return Err(
        cliFactoryError('UNSUPPORTED_FRAMEWORK', `Unsupported framework: ${framework}`, {
          framework,
          operation: 'createCLI',
          availableFrameworks: ['readline', 'ink', 'hybrid'],
        })
      )
  }
}

/**
 * Create a validated CLI instance
 *
 * @param config - CLI configuration with validation
 * @returns Result containing the validated CLI instance or error
 */
export function createValidatedCLI(
  config: Partial<CLIConfigWithFramework> = {}
): Result<ICLIFramework, QiError> {
  const framework = config.framework || 'readline'

  // Check framework support first
  const supportResult = checkFrameworkSupport(framework)

  return match(
    () => {
      switch (framework) {
        case 'readline':
          return createValidatedReadlineCLI(config)

        case 'ink':
          return createValidatedInkCLI(config)

        case 'hybrid':
          return Err(
            cliFactoryError(
              'HYBRID_VALIDATION_NOT_SUPPORTED',
              'Hybrid CLI validation not supported. Use createCLIAsync() instead.',
              {
                framework: 'hybrid',
                operation: 'createValidatedCLI',
              }
            )
          )

        default:
          return Err(
            cliFactoryError('UNSUPPORTED_FRAMEWORK', `Unsupported framework: ${framework}`, {
              framework,
              operation: 'createValidatedCLI',
            })
          )
      }
    },
    (error) => Err(error),
    supportResult
  )
}

/**
 * Create a CLI instance with async initialization
 *
 * @param config - CLI configuration
 * @returns Promise resolving to Result with CLI instance or error
 */
export async function createCLIAsync(
  config: Partial<CLIConfigWithFramework> = {}
): Promise<Result<ICLIFramework, QiError>> {
  const logger = createDebugLogger('CLIFactory')
  const framework = config.framework || 'readline'
  logger.trace(
    `createCLIAsync - using framework: ${framework} (config.framework: ${config.framework})`
  )

  // If user explicitly requested a framework, it MUST work - NO FALLBACKS
  const _explicitFrameworkRequested = !!config.framework

  switch (framework) {
    case 'readline':
      return await createReadlineCLIAsync(config)

    case 'ink':
      return await createInkCLIAsync(config)

    case 'hybrid':
      logger.trace('User explicitly requested hybrid - MUST succeed or exit')
      return await createHybridCLIAsync(config)

    default:
      return Err(
        cliFactoryError('UNSUPPORTED_FRAMEWORK', `Unsupported framework: ${framework}`, {
          framework,
          operation: 'createCLIAsync',
        })
      )
  }
}

/**
 * Get framework support information
 */
export function getFrameworkSupport(): Record<CLIFramework, any> {
  return {
    readline: checkReadlineSupport(),
    ink: checkInkSupport(),
    hybrid: checkHybridSupport(),
  }
}

/**
 * Check if a specific framework is supported in the current environment
 */
export function checkFrameworkSupport(framework: CLIFramework): Result<void, QiError> {
  switch (framework) {
    case 'readline': {
      const support = checkReadlineSupport()
      if (!support.terminal) {
        return Err(
          cliFactoryError('READLINE_NOT_SUPPORTED', 'Readline framework requires TTY support', {
            framework,
            supportCheck: support,
          })
        )
      }
      return Ok(void 0)
    }

    case 'ink': {
      const support = checkInkSupport()
      if (!support.available) {
        return Err(
          cliFactoryError(
            'INK_NOT_SUPPORTED',
            'Ink framework is not available. Run: bun add ink ink-text-input',
            { framework, supportCheck: support }
          )
        )
      }
      return Ok(void 0)
    }

    case 'hybrid': {
      const support = checkHybridSupport()
      if (!support.available) {
        return Err(
          cliFactoryError('HYBRID_NOT_SUPPORTED', support.reason, {
            framework,
            supportCheck: support,
          })
        )
      }
      return Ok(void 0)
    }

    default:
      return Err(
        cliFactoryError('UNKNOWN_FRAMEWORK', `Unknown framework: ${framework}`, { framework })
      )
  }
}

/**
 * Recommend the best framework for the current environment
 */
export function recommendFramework(): {
  framework: CLIFramework
  reason: string
  alternatives: CLIFramework[]
} {
  const support = getFrameworkSupport()

  // Check if Hybrid is available (best of both worlds)
  if (support.hybrid.available) {
    return {
      framework: 'hybrid',
      reason: 'Combines readline cursor control with Ink rich UI - Claude Code-style navigation',
      alternatives: ['ink', 'readline'],
    }
  }

  // Check if Ink is available and terminal supports rich UI
  if (support.ink.available && support.readline.terminal && support.readline.colors) {
    return {
      framework: 'ink',
      reason: 'Rich React-based UI with excellent terminal support',
      alternatives: ['readline'],
    }
  }

  // Default to readline (always available)
  return {
    framework: 'readline',
    reason: 'Zero dependencies, always available',
    alternatives: ['ink'],
  }
}

// Framework-specific factories

function createInkCLI(
  _config: Partial<CLIConfig> = {},
  _messageQueue?: any
): Result<ICLIFramework, QiError> {
  return Err(
    cliFactoryError(
      'INK_SYNC_NOT_SUPPORTED',
      'Ink CLI requires async initialization due to dynamic imports. Use createInkCLIAsync() instead.',
      { framework: 'ink', operation: 'createInkCLI' }
    )
  )
}

function createValidatedInkCLI(config: Partial<CLIConfig> = {}): Result<ICLIFramework, QiError> {
  // Run validation first
  const validationResult = checkFrameworkSupport('ink')

  return match(
    () => createInkCLI(config),
    (error) => Err(error),
    validationResult
  )
}

async function createInkCLIAsync(
  config: Partial<CLIConfig> = {},
  messageQueue?: any
): Promise<Result<ICLIFramework, QiError>> {
  try {
    // Check if Ink is available
    const support = checkInkSupport()
    if (!support.available) {
      return Err(
        cliFactoryError(
          'INK_NOT_AVAILABLE',
          `Ink framework not available: ${support.reason}. Install with: bun add ${support.packages?.join(' ')}`,
          { framework: 'ink', operation: 'createInkCLIAsync', supportCheck: support }
        )
      )
    }

    // TEMP: Use static import - InkCLIFramework already imported at top
    // const { InkCLIFramework } = await import('../frameworks/ink/InkCLIFramework');

    // Create actual Ink CLI implementation with messageQueue and stateManager from config
    const cli = new InkCLIFramework(config, config.messageQueue || messageQueue)

    return Ok(cli)
  } catch (error: any) {
    return Err(
      cliFactoryError('INK_CREATION_FAILED', `Failed to create Ink CLI: ${error.message}`, {
        framework: 'ink',
        operation: 'createInkCLIAsync',
      })
    )
  }
}

// Support checking functions

function checkInkSupport(): { available: boolean; reason: string; packages?: string[] } {
  // For bundling compatibility, assume Ink is available if in development
  // Actual runtime checks will happen in dynamic imports
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
    packages: ['ink', 'ink-text-input'],
  }
}

function checkHybridSupport(): { available: boolean; reason: string; dependencies?: string[] } {
  const readlineSupport = checkReadlineSupport()
  const inkSupport = checkInkSupport()

  // Hybrid requires both readline TTY support and Ink packages
  if (!readlineSupport.terminal) {
    return {
      available: false,
      reason: 'Hybrid framework requires terminal (TTY) support for readline input control',
      dependencies: ['Terminal/TTY environment'],
    }
  }

  if (!inkSupport.available) {
    return {
      available: false,
      reason: `Hybrid framework requires Ink packages for rich UI: ${inkSupport.packages?.join(' ')}`,
      dependencies: inkSupport.packages,
    }
  }

  return {
    available: true,
    reason: 'Hybrid framework available - combines readline input control with Ink rich UI',
  }
}

// Use the imported function

/**
 * Get all available frameworks
 */
export function getAvailableFrameworks(): CLIFramework[] {
  const frameworks: CLIFramework[] = []

  // Readline is always available
  frameworks.push('readline')

  // Check optional frameworks
  if (checkInkSupport().available) {
    frameworks.push('ink')
  }

  if (checkHybridSupport().available) {
    frameworks.push('hybrid')
  }

  return frameworks
}

// Framework-specific Hybrid factories

// DELETED: Sync createHybridCLI - always failed anyway

// DELETED: createValidatedHybridCLI - another fallback function

async function createHybridCLIAsync(
  config: Partial<CLIConfig> = {},
  messageQueue?: any
): Promise<Result<ICLIFramework, QiError>> {
  const logger = createDebugLogger('HybridCLIFactory')
  logger.trace('createHybridCLIAsync called - attempting to create hybrid CLI')
  try {
    // Check if both readline and Ink are available for hybrid mode
    const readlineSupport = checkReadlineSupport()
    const inkSupport = checkInkSupport()

    if (!readlineSupport.terminal) {
      return Err(
        cliFactoryError(
          'HYBRID_READLINE_NOT_AVAILABLE',
          'Hybrid framework requires terminal (TTY) support for readline',
          { framework: 'hybrid', operation: 'createHybridCLIAsync', supportCheck: readlineSupport }
        )
      )
    }

    if (!inkSupport.available) {
      return Err(
        cliFactoryError(
          'HYBRID_INK_NOT_AVAILABLE',
          `Hybrid framework requires Ink packages: ${inkSupport.packages?.join(' ')}`,
          { framework: 'hybrid', operation: 'createHybridCLIAsync', supportCheck: inkSupport }
        )
      )
    }

    // TEMP: Use static imports for development - avoids Zod caching conflicts with bun run
    // TODO: Revert to dynamic imports for binary compilation
    // Classes are already imported statically at top of file

    // Create hybrid CLI implementation with shared message queue and state manager
    logger.trace('createHybridCLIAsync - config keys:', Object.keys(config))
    logger.trace('createHybridCLIAsync - stateManager:', !!config.stateManager)
    logger.trace('creating HybridCLIFramework instance')
    const cli = new HybridCLIFramework(config, config.messageQueue || messageQueue)
    logger.trace('HybridCLIFramework created successfully:', cli.constructor.name)

    return Ok(cli)
  } catch (error: any) {
    logger.error('Hybrid framework creation FAILED:', error.message)
    logger.error('Error details:', error)
    return Err(
      cliFactoryError('HYBRID_CREATION_FAILED', `Failed to create Hybrid CLI: ${error.message}`, {
        framework: 'hybrid',
        operation: 'createHybridCLIAsync',
      })
    )
  }
}

// Export the framework-specific factories for direct use
export { createInkCLI }
// createHybridCLI deleted - use createCLIAsync() instead
