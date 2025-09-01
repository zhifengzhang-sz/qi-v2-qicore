/**
 * CLI Container - Dependency injection for CLI components
 *
 * This module exports the dependency injection container implementation
 * with QiCore-based error handling and lifecycle management.
 */

// Re-export container interfaces
export type {
  CLIFactory,
  ContainerConfig,
  ContainerFactory,
  FrameworkRegistration,
  ICLIContainer,
  ICLIContainerBuilder,
  IFrameworkRegistry,
  ServiceFactory,
  ServiceLifecycle,
  ServiceRegistration,
} from '../abstractions/ICLIContainer'
// Container implementation
export { CLIContainer } from './CLIContainer'
