/**
 * CLI Dependency Injection Container interface
 *
 * Provides a framework-agnostic container for managing CLI dependencies.
 * Supports service registration and resolution with proper lifecycle management.
 */

import type { QiError, Result } from '@qi/base'
import type { CLIConfig, ICLIFramework } from './ICLIFramework'

/**
 * Service factory function type
 */
export type ServiceFactory<T = unknown> = () => T | Promise<T>

/**
 * Service lifecycle management
 */
export interface ServiceLifecycle {
  singleton?: boolean
  lazy?: boolean
  initializer?: (instance: unknown) => void | Promise<void>
  destroyer?: (instance: unknown) => void | Promise<void>
}

/**
 * Service registration configuration
 */
export interface ServiceRegistration<T = unknown> extends ServiceLifecycle {
  factory: ServiceFactory<T>
  dependencies?: string[]
}

/**
 * Container configuration
 */
export interface ContainerConfig {
  enableDebug?: boolean
  enableValidation?: boolean
  maxResolutionDepth?: number
}

/**
 * Dependency injection container interface
 */
export interface ICLIContainer {
  /**
   * Register a service with the container
   */
  register<T>(
    key: string,
    factory: ServiceFactory<T>,
    lifecycle?: ServiceLifecycle
  ): Result<void, QiError>

  /**
   * Register a service with full configuration
   */
  registerService<T>(key: string, registration: ServiceRegistration<T>): Result<void, QiError>

  /**
   * Resolve a service from the container
   */
  resolve<T>(key: string): Result<T, QiError>

  /**
   * Resolve a service asynchronously
   */
  resolveAsync<T>(key: string): Promise<Result<T, QiError>>

  /**
   * Check if a service is registered
   */
  isRegistered(key: string): boolean

  /**
   * Get all registered service keys
   */
  getRegisteredServices(): string[]

  /**
   * Unregister a service
   */
  unregister(key: string): Result<void, QiError>

  /**
   * Clear all registered services
   */
  clear(): Result<void, QiError>

  /**
   * Create a child container
   */
  createChild(): Result<ICLIContainer, QiError>

  /**
   * Get container configuration
   */
  getConfig(): ContainerConfig

  /**
   * Update container configuration
   */
  updateConfig(config: Partial<ContainerConfig>): Result<void, QiError>

  /**
   * Create CLI instance with resolved dependencies
   */
  createCLI(config: CLIConfig): Result<ICLIFramework, QiError>

  /**
   * Validate all service dependencies
   */
  validateDependencies(): Result<void, QiError>

  /**
   * Get dependency graph
   */
  getDependencyGraph(): Record<string, string[]>

  /**
   * Dispose of all singleton instances and clean up
   */
  dispose(): Promise<Result<void, QiError>>
}

/**
 * Container builder interface for fluent configuration
 */
export interface ICLIContainerBuilder {
  /**
   * Configure the container
   */
  configure(config: Partial<ContainerConfig>): ICLIContainerBuilder

  /**
   * Register a service
   */
  registerService<T>(
    key: string,
    factory: ServiceFactory<T>,
    lifecycle?: ServiceLifecycle
  ): ICLIContainerBuilder

  /**
   * Register multiple services at once
   */
  registerServices(services: Record<string, ServiceRegistration>): ICLIContainerBuilder

  /**
   * Build the container
   */
  build(): Result<ICLIContainer, QiError>

  /**
   * Build and validate the container
   */
  buildAndValidate(): Result<ICLIContainer, QiError>
}

/**
 * Factory function types for framework creation
 */
export type CLIFactory = (config: CLIConfig) => Result<ICLIFramework, QiError>
export type ContainerFactory = (config?: Partial<ContainerConfig>) => Result<ICLIContainer, QiError>

/**
 * Framework registration information
 */
export interface FrameworkRegistration {
  name: string
  factory: CLIFactory
  dependencies: string[]
  description?: string
  version?: string
}

/**
 * Framework registry interface
 */
export interface IFrameworkRegistry {
  /**
   * Register a CLI framework
   */
  registerFramework(id: string, registration: FrameworkRegistration): Result<void, QiError>

  /**
   * Get registered framework
   */
  getFramework(id: string): Result<FrameworkRegistration, QiError>

  /**
   * List all registered frameworks
   */
  listFrameworks(): FrameworkRegistration[]

  /**
   * Create CLI using registered framework
   */
  createCLI(frameworkId: string, config: CLIConfig): Result<ICLIFramework, QiError>
}
