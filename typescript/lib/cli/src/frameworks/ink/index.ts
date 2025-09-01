/**
 * Ink CLI Framework
 *
 * React-based CLI implementation using Ink framework
 */

// React Components
export { InputBox } from './components/InputBox'
export { MainLayout } from './components/MainLayout'
export {
  createOutputMessage,
  OutputDisplay,
  type OutputMessage,
} from './components/OutputDisplay'
export { StateIndicator } from './components/StateIndicator'
// Main Ink CLI Framework
export { createInkCLIImpl, InkCLIFramework } from './InkCLIFramework'

// Legacy components (deprecated)
// export { InkCLIApplication, createInkCLI } from './InkCLI' // DEPRECATED
