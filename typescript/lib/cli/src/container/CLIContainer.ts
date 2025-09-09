/**
 * CLI Dependency Injection Container
 *
 * QiCore-based implementation of dependency injection with robust error handling
 * and lifecycle management for CLI components.
 */

import { create, Err, match, Ok, type QiError, type Result } from '@qi/base'
import type {
  ContainerConfig,
  ICLIContainer,
  ServiceFactory,
  ServiceLifecycle,
  ServiceRegistration,
} from '../abstractions/ICLIContainer'
import type { CLIConfig, ICLIFramework } from '../abstractions/ICLIFramework'

/**
 * Container error types
 */
interface ContainerError extends QiError {
  context: {
    serviceKey?: string
    operation?: string
    registeredServices?: string[]
    dependencyChain?: string[]
    maxDepth?: number
    currentDepth?: number
  }
}

const containerError = (
  code: string,
  message: string,
  context: ContainerError['context'] = {}
): ContainerError => create(code, message, 'SYSTEM', context) as ContainerError

/**
 * Service registration internal interface
 */
interface InternalServiceRegistration<T = any> extends ServiceRegistration<T> {
  instance?: T
  isResolving?: boolean
  resolveCount: number
  lastResolvedAt?: Date
}

/**
 * Default container configuration
 */
const DEFAULT_CONFIG: ContainerConfig = {
  enableDebug: false,
  enableValidation: true,
  maxResolutionDepth: 10,
}

/**
 * QiCore-based dependency injection container
 * Provides robust service registration, resolution, and lifecycle management
 */
export class CLIContainer implements ICLIContainer {
  private config: ContainerConfig
  private services: Map<string, InternalServiceRegistration> = new Map()
  private parentContainer: CLIContainer | null = null
  private childContainers: Set<CLIContainer> = new Set()
  private isDisposed = false
  private singletonInstances: Map<string, any> = new Map()

  constructor(config: Partial<ContainerConfig> = {}, parent?: CLIContainer) {
    this.config = { ...DEFAULT_CONFIG, ...config }
    this.parentContainer = parent || null

    if (parent) {
      parent.addChild(this)
    }
  }

  /**
   * Register a service with the container
   */
  register<T>(
    key: string,
    factory: ServiceFactory<T>,
    lifecycle?: ServiceLifecycle
  ): Result<void, QiError> {
    const registration: ServiceRegistration<T> = {
      factory,
      ...lifecycle,
    }

    return this.registerService(key, registration)
  }

  /**
   * Register a service with full configuration
   */
  registerService<T>(key: string, registration: ServiceRegistration<T>): Result<void, QiError> {
    if (this.isDisposed) {
      return Err(
        containerError('CONTAINER_DISPOSED', 'Cannot register service on disposed container', {
          serviceKey: key,
          operation: 'register',
        })
      )
    }

    const keyValidation = this.validateServiceKey(key)

    return match(
      () => {
        const registrationValidation = this.validateRegistration(registration)

        return match(
          () => {
            // Create internal registration
            const internalRegistration: InternalServiceRegistration<T> = {
              ...registration,
              resolveCount: 0,
            }

            this.services.set(key, internalRegistration)

            if (this.config.enableDebug) {
              console.debug(`CLIContainer: Registered service '${key}'`)
            }

            return Ok(void 0)
          },
          (error): Result<void, QiError> => Err(error),
          registrationValidation
        )
      },
      (error): Result<void, QiError> => Err(error),
      keyValidation
    )
  }

  /**
   * Resolve a service from the container
   */
  resolve<T>(key: string): Result<T, QiError> {
    if (this.isDisposed) {
      return Err(
        containerError('CONTAINER_DISPOSED', 'Cannot resolve service from disposed container', {
          serviceKey: key,
          operation: 'resolve',
        })
      )
    }

    return this.resolveWithDepth(key, [], 0)
  }

  /**
   * Resolve a service asynchronously
   */
  async resolveAsync<T>(key: string): Promise<Result<T, QiError>> {
    const resolveResult = this.resolve<T>(key)

    return await match(
      async (service): Promise<Result<T, QiError>> => {
        // If service is a promise, await it
        if (service && typeof service === 'object' && 'then' in service) {
          try {
            const resolved = await (service as unknown as Promise<unknown>)
            return Ok(resolved as T)
          } catch (error) {
            return Err(
              containerError(
                'ASYNC_RESOLUTION_FAILED',
                `Async service resolution failed: ${error instanceof Error ? error.message : 'Unknown error'}`,
                { serviceKey: key, operation: 'resolveAsync' }
              )
            )
          }
        }

        return Ok(service)
      },
      async (error: QiError): Promise<Result<T, QiError>> => Err(error),
      resolveResult
    )
  }

  /**
   * Check if a service is registered
   */
  isRegistered(key: string): boolean {
    if (this.services.has(key)) {
      return true
    }

    // Check parent container
    return this.parentContainer?.isRegistered(key) || false
  }

  /**
   * Get all registered service keys
   */
  getRegisteredServices(): string[] {
    const keys = new Set<string>()

    // Add own services
    for (const key of this.services.keys()) {
      keys.add(key)
    }

    // Add parent services
    if (this.parentContainer) {
      for (const key of this.parentContainer.getRegisteredServices()) {
        keys.add(key)
      }
    }

    return Array.from(keys).sort()
  }

  /**
   * Unregister a service
   */
  unregister(key: string): Result<void, QiError> {
    if (this.isDisposed) {
      return Err(
        containerError('CONTAINER_DISPOSED', 'Cannot unregister service from disposed container', {
          serviceKey: key,
          operation: 'unregister',
        })
      )
    }

    if (!this.services.has(key)) {
      return Err(
        containerError(
          'SERVICE_NOT_FOUND',
          `Service '${key}' is not registered in this container`,
          {
            serviceKey: key,
            operation: 'unregister',
            registeredServices: Array.from(this.services.keys()),
          }
        )
      )
    }

    // Clean up singleton instance if exists
    const registration = this.services.get(key)
    if (registration?.singleton && this.singletonInstances.has(key)) {
      const instance = this.singletonInstances.get(key)

      // Call destroyer if available
      if (registration.destroyer) {
        try {
          registration.destroyer(instance)
        } catch (error) {
          console.warn(`Error in destroyer for service '${key}':`, error)
        }
      }

      this.singletonInstances.delete(key)
    }

    this.services.delete(key)

    if (this.config.enableDebug) {
      console.debug(`CLIContainer: Unregistered service '${key}'`)
    }

    return Ok(void 0)
  }

  /**
   * Clear all registered services
   */
  clear(): Result<void, QiError> {
    if (this.isDisposed) {
      return Err(
        containerError('CONTAINER_DISPOSED', 'Cannot clear disposed container', {
          operation: 'clear',
        })
      )
    }

    // Dispose of all singletons
    for (const [key, registration] of this.services.entries()) {
      if (registration.singleton && this.singletonInstances.has(key)) {
        const instance = this.singletonInstances.get(key)

        if (registration.destroyer) {
          try {
            registration.destroyer(instance)
          } catch (error) {
            console.warn(`Error in destroyer for service '${key}':`, error)
          }
        }
      }
    }

    this.services.clear()
    this.singletonInstances.clear()

    if (this.config.enableDebug) {
      console.debug('CLIContainer: Cleared all services')
    }

    return Ok(void 0)
  }

  /**
   * Create a child container
   */
  createChild(): Result<ICLIContainer, QiError> {
    if (this.isDisposed) {
      return Err(
        containerError('CONTAINER_DISPOSED', 'Cannot create child of disposed container', {
          operation: 'createChild',
        })
      )
    }

    try {
      const child = new CLIContainer(this.config, this)
      return Ok(child)
    } catch (error) {
      return Err(
        containerError(
          'CHILD_CREATION_FAILED',
          `Failed to create child container: ${error instanceof Error ? error.message : 'Unknown error'}`,
          { operation: 'createChild' }
        )
      )
    }
  }

  /**
   * Get container configuration
   */
  getConfig(): ContainerConfig {
    return { ...this.config }
  }

  /**
   * Update container configuration
   */
  updateConfig(config: Partial<ContainerConfig>): Result<void, QiError> {
    if (this.isDisposed) {
      return Err(
        containerError('CONTAINER_DISPOSED', 'Cannot update config of disposed container', {
          operation: 'updateConfig',
        })
      )
    }

    this.config = { ...this.config, ...config }
    return Ok(void 0)
  }

  /**
   * Create CLI instance with resolved dependencies
   */
  createCLI(_config: CLIConfig): Result<ICLIFramework, QiError> {
    if (this.isDisposed) {
      return Err(
        containerError('CONTAINER_DISPOSED', 'Cannot create CLI from disposed container', {
          operation: 'createCLI',
        })
      )
    }

    // This will be implemented by the factory functions
    // The container itself doesn't know how to create CLI instances
    return Err(
      containerError(
        'CLI_CREATION_NOT_IMPLEMENTED',
        'CLI creation must be handled by framework factories',
        { operation: 'createCLI' }
      )
    )
  }

  /**
   * Validate all service dependencies
   */
  validateDependencies(): Result<void, QiError> {
    if (this.isDisposed) {
      return Err(
        containerError('CONTAINER_DISPOSED', 'Cannot validate dependencies of disposed container', {
          operation: 'validate',
        })
      )
    }

    for (const [key, registration] of this.services.entries()) {
      if (registration.dependencies) {
        for (const depKey of registration.dependencies) {
          if (!this.isRegistered(depKey)) {
            return Err(
              containerError(
                'MISSING_DEPENDENCY',
                `Service '${key}' depends on unregistered service '${depKey}'`,
                {
                  serviceKey: key,
                  operation: 'validate',
                  registeredServices: this.getRegisteredServices(),
                }
              )
            )
          }
        }
      }
    }

    return Ok(void 0)
  }

  /**
   * Get dependency graph
   */
  getDependencyGraph(): Record<string, string[]> {
    const graph: Record<string, string[]> = {}

    for (const [key, registration] of this.services.entries()) {
      graph[key] = registration.dependencies || []
    }

    return graph
  }

  /**
   * Dispose of all singleton instances and clean up
   */
  async dispose(): Promise<Result<void, QiError>> {
    if (this.isDisposed) {
      return Ok(void 0)
    }

    try {
      // Dispose of child containers first
      for (const child of this.childContainers) {
        await child.dispose()
      }

      this.childContainers.clear()

      // Dispose of singleton instances
      for (const [key, registration] of this.services.entries()) {
        if (registration.singleton && this.singletonInstances.has(key)) {
          const instance = this.singletonInstances.get(key)

          if (registration.destroyer) {
            try {
              const result = registration.destroyer(instance)

              // Handle async destroyers
              if (result && typeof result === 'object' && 'then' in result) {
                await result
              }
            } catch (error) {
              console.warn(`Error in destroyer for service '${key}':`, error)
            }
          }
        }
      }

      this.services.clear()
      this.singletonInstances.clear()
      this.isDisposed = true

      // Remove from parent
      if (this.parentContainer) {
        this.parentContainer.removeChild(this)
      }

      if (this.config.enableDebug) {
        console.debug('CLIContainer: Disposed')
      }

      return Ok(void 0)
    } catch (error) {
      return Err(
        containerError(
          'DISPOSAL_FAILED',
          `Container disposal failed: ${error instanceof Error ? error.message : 'Unknown error'}`,
          { operation: 'dispose' }
        )
      )
    }
  }

  // Private methods

  private resolveWithDepth<T>(
    key: string,
    dependencyChain: string[],
    currentDepth: number
  ): Result<T, QiError> {
    // Check depth limit
    if (currentDepth >= this.config.maxResolutionDepth!) {
      return Err(
        containerError(
          'MAX_RESOLUTION_DEPTH_EXCEEDED',
          `Maximum resolution depth of ${this.config.maxResolutionDepth} exceeded`,
          {
            serviceKey: key,
            operation: 'resolve',
            dependencyChain,
            maxDepth: this.config.maxResolutionDepth,
            currentDepth,
          }
        )
      )
    }

    // Check for circular dependencies
    if (dependencyChain.includes(key)) {
      return Err(
        containerError(
          'CIRCULAR_DEPENDENCY',
          `Circular dependency detected: ${[...dependencyChain, key].join(' -> ')}`,
          { serviceKey: key, operation: 'resolve', dependencyChain }
        )
      )
    }

    // Try to resolve from this container
    const registration = this.services.get(key)

    if (registration) {
      return this.createInstance<T>(key, registration, dependencyChain, currentDepth)
    }

    // Try parent container
    if (this.parentContainer) {
      return this.parentContainer.resolveWithDepth<T>(key, dependencyChain, currentDepth + 1)
    }

    return Err(
      containerError('SERVICE_NOT_FOUND', `Service '${key}' not found`, {
        serviceKey: key,
        operation: 'resolve',
        registeredServices: this.getRegisteredServices(),
      })
    )
  }

  private createInstance<T>(
    key: string,
    registration: InternalServiceRegistration,
    dependencyChain: string[],
    currentDepth: number
  ): Result<T, QiError> {
    // Check if singleton and already created
    if (registration.singleton && this.singletonInstances.has(key)) {
      const instance = this.singletonInstances.get(key)
      registration.resolveCount++
      registration.lastResolvedAt = new Date()
      return Ok(instance)
    }

    // Prevent re-entrant resolution
    if (registration.isResolving) {
      return Err(
        containerError(
          'REENTRANT_RESOLUTION',
          `Re-entrant resolution detected for service '${key}'`,
          { serviceKey: key, operation: 'resolve', dependencyChain }
        )
      )
    }

    try {
      registration.isResolving = true

      // Resolve dependencies first
      if (registration.dependencies) {
        for (const depKey of registration.dependencies) {
          const depResult = this.resolveWithDepth(
            depKey,
            [...dependencyChain, key],
            currentDepth + 1
          )

          if (!depResult.tag || depResult.tag === 'failure') {
            return depResult as Result<T, QiError>
          }
        }
      }

      // Create instance
      const instance = registration.factory()

      // Handle async factories
      if (instance && typeof instance === 'object' && 'then' in instance) {
        return Err(
          containerError(
            'ASYNC_FACTORY_NOT_SUPPORTED',
            `Async factories not supported in synchronous resolve. Use resolveAsync instead.`,
            { serviceKey: key, operation: 'resolve' }
          )
        )
      }

      // Run initializer if available
      if (registration.initializer) {
        try {
          registration.initializer(instance)
        } catch (error) {
          return Err(
            containerError(
              'INITIALIZER_FAILED',
              `Initializer failed for service '${key}': ${error instanceof Error ? error.message : 'Unknown error'}`,
              { serviceKey: key, operation: 'initialize' }
            )
          )
        }
      }

      // Store singleton instance
      if (registration.singleton) {
        this.singletonInstances.set(key, instance)
      }

      registration.resolveCount++
      registration.lastResolvedAt = new Date()

      if (this.config.enableDebug) {
        console.debug(
          `CLIContainer: Resolved service '${key}' (count: ${registration.resolveCount})`
        )
      }

      return Ok(instance as T)
    } catch (error) {
      return Err(
        containerError(
          'FACTORY_FAILED',
          `Factory failed for service '${key}': ${error instanceof Error ? error.message : 'Unknown error'}`,
          { serviceKey: key, operation: 'factory' }
        )
      )
    } finally {
      registration.isResolving = false
    }
  }

  private validateServiceKey(key: string): Result<void, QiError> {
    if (!key || typeof key !== 'string') {
      return Err(
        containerError('INVALID_SERVICE_KEY', 'Service key must be a non-empty string', {
          serviceKey: key,
          operation: 'validateKey',
        })
      )
    }

    if (key.length > 100) {
      return Err(
        containerError('SERVICE_KEY_TOO_LONG', 'Service key must not exceed 100 characters', {
          serviceKey: key,
          operation: 'validateKey',
        })
      )
    }

    return Ok(void 0)
  }

  private validateRegistration<T>(registration: ServiceRegistration<T>): Result<void, QiError> {
    if (!registration.factory || typeof registration.factory !== 'function') {
      return Err(
        containerError('INVALID_FACTORY', 'Service factory must be a function', {
          operation: 'validateRegistration',
        })
      )
    }

    if (registration.dependencies && !Array.isArray(registration.dependencies)) {
      return Err(
        containerError('INVALID_DEPENDENCIES', 'Service dependencies must be an array', {
          operation: 'validateRegistration',
        })
      )
    }

    return Ok(void 0)
  }

  private addChild(child: CLIContainer): void {
    this.childContainers.add(child)
  }

  private removeChild(child: CLIContainer): void {
    this.childContainers.delete(child)
  }

  /**
   * Get container statistics
   */
  getStats(): {
    serviceCount: number
    singletonCount: number
    childCount: number
    isDisposed: boolean
    totalResolveCount: number
  } {
    let totalResolveCount = 0

    for (const registration of this.services.values()) {
      totalResolveCount += registration.resolveCount
    }

    return {
      serviceCount: this.services.size,
      singletonCount: this.singletonInstances.size,
      childCount: this.childContainers.size,
      isDisposed: this.isDisposed,
      totalResolveCount,
    }
  }
}
