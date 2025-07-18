/**
 * QiCore Foundation - Main Entry Point
 *
 * TypeScript implementation of QiCore mathematical foundation types and infrastructure services.
 * Version 0.3.4 includes qi/base (Result<T>, QiError) with pure functional patterns
 * and qi/core (Config, Logger, Cache) with modern TypeScript patterns.
 */

// Re-export everything from base module
export * from './base/index.js'

// Re-export everything from core module except conflicting exports
export * from './core/index.js'

// Explicitly re-export loggerError from core (takes precedence)
export { loggerError } from './core/index.js'
