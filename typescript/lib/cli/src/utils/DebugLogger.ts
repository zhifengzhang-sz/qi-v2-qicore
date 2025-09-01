/**
 * Debug Logger Utility - Conditional Debug Logging
 *
 * Provides consistent debug logging across the application that can be
 * enabled/disabled via debug flag or environment variable.
 */

/**
 * Global debug state - set once during app initialization
 */
let globalDebugEnabled = false

/**
 * Initialize debug logging state
 */
export function initializeDebugLogging(debugEnabled: boolean): void {
  globalDebugEnabled = debugEnabled || process.env.QI_CLI_DEBUG === 'true'
}

/**
 * Check if debug logging is enabled
 */
export function isDebugEnabled(): boolean {
  return globalDebugEnabled
}

/**
 * Debug logging function - only logs if debug is enabled
 */
export function debugLog(message: string, ...args: any[]): void {
  if (globalDebugEnabled) {
    console.log(message, ...args)
  }
}

/**
 * Debug error logging function - only logs if debug is enabled
 */
export function debugError(message: string, ...args: any[]): void {
  if (globalDebugEnabled) {
    console.error(message, ...args)
  }
}

/**
 * Debug warning logging function - only logs if debug is enabled
 */
export function debugWarn(message: string, ...args: any[]): void {
  if (globalDebugEnabled) {
    console.warn(message, ...args)
  }
}

/**
 * Create a debug logger for a specific component
 */
export function createDebugLogger(componentName: string) {
  return {
    log: (message: string, ...args: any[]) => debugLog(`[${componentName}] ${message}`, ...args),

    error: (message: string, ...args: any[]) =>
      debugError(`[${componentName}] ${message}`, ...args),

    warn: (message: string, ...args: any[]) => debugWarn(`[${componentName}] ${message}`, ...args),

    trace: (message: string, ...args: any[]) =>
      debugLog(`🔍 [${componentName}] ${message}`, ...args),

    info: (message: string, ...args: any[]) =>
      debugLog(`ℹ️  [${componentName}] ${message}`, ...args),
  }
}
