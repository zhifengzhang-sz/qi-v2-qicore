/**
 * CLI Factories - Framework creation functions
 *
 * This module exports factory functions for creating CLI instances
 * with different terminal frameworks and configurations.
 */

// Re-export core interfaces
export type {
  CLIConfig,
  CLIMode,
  CLIState,
  ICLIFramework,
  MessageType,
} from '../abstractions/ICLIFramework'
// Framework-agnostic factory
export {
  type CLIConfigWithFramework,
  type CLIFramework,
  checkFrameworkSupport,
  createCLI,
  createCLIAsync,
  createValidatedCLI,
  getAvailableFrameworks,
  getFrameworkSupport,
  recommendFramework,
} from './createCLI'
// Readline framework factory
export {
  checkReadlineSupport,
  createReadlineCLI,
  createReadlineCLIAsync,
  createValidatedReadlineCLI,
  getDefaultReadlineConfig,
} from './createReadlineCLI'
