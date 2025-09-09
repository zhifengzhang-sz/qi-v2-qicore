/**
 * CLI Configuration Loader
 *
 * Loads CLI configuration from multiple sources with proper priority:
 * 1. Command line arguments (highest priority)
 * 2. Environment variables
 * 3. Configuration file
 * 4. Default values (lowest priority)
 */

import { existsSync, readFileSync } from 'node:fs'
import YAML from 'js-yaml'
import type { CLIConfig } from '../abstractions/ICLIFramework'

export type CLIFramework = 'readline' | 'ink' | 'hybrid'

export interface CLIConfigWithFramework extends CLIConfig {
  framework: CLIFramework
  stateManager?: any // StateManager for UI updates
  messageQueue?: any // Message queue for communication
}

interface ConfigLoaderOptions {
  configPath?: string
  args?: string[]
  env?: Record<string, string | undefined>
}

/**
 * Default configuration
 */
const defaultConfig: CLIConfigWithFramework = {
  framework: 'readline',
  enableHotkeys: true,
  enableModeIndicator: true,
  enableProgressDisplay: true,
  enableStreaming: true,
  prompt: '> ',
  colors: true,
  animations: true,
  historySize: 100,
  autoComplete: false,
  streamingThrottle: 10,
  maxBufferSize: 1000000,
  debug: false,
}

/**
 * Environment variable mappings
 */
const envMappings = {
  QI_CLI_FRAMEWORK: 'framework',
  QI_CLI_DEBUG: 'debug',
  QI_CLI_ENABLE_HOTKEYS: 'enableHotkeys',
  QI_CLI_ENABLE_STREAMING: 'enableStreaming',
  QI_CLI_ENABLE_PROGRESS: 'enableProgressDisplay',
  QI_CLI_COLORS: 'colors',
  QI_CLI_ANIMATIONS: 'animations',
  QI_CLI_PROMPT: 'prompt',
  QI_CLI_HISTORY_SIZE: 'historySize',
  QI_CLI_STREAMING_THROTTLE: 'streamingThrottle',
  QI_CLI_MAX_BUFFER_SIZE: 'maxBufferSize',
} as const

/**
 * Command line argument mappings
 */
const argMappings = {
  '--framework': 'framework',
  '-f': 'framework',
  '--debug': 'debug',
  '-d': 'debug',
  '--no-hotkeys': { key: 'enableHotkeys', value: false },
  '--no-streaming': { key: 'enableStreaming', value: false },
  '--no-progress': { key: 'enableProgressDisplay', value: false },
  '--no-colors': { key: 'colors', value: false },
  '--no-animations': { key: 'animations', value: false },
  '--prompt': 'prompt',
  '--history-size': 'historySize',
  '--streaming-throttle': 'streamingThrottle',
  '--max-buffer-size': 'maxBufferSize',
} as const

/**
 * Load CLI configuration from multiple sources
 */
export function loadCLIConfig(options: ConfigLoaderOptions = {}): CLIConfigWithFramework {
  let config = { ...defaultConfig }

  // 1. Load from configuration file (lowest priority)
  config = mergeConfigFromFile(config, options.configPath)

  // 2. Load from environment variables
  config = mergeConfigFromEnv(config, options.env || process.env)

  // 3. Load from command line arguments (highest priority)
  config = mergeConfigFromArgs(config, options.args || process.argv.slice(2))

  return validateConfig(config)
}

/**
 * Load configuration from file
 */
function mergeConfigFromFile(
  config: CLIConfigWithFramework,
  configPath?: string
): CLIConfigWithFramework {
  const possiblePaths = [
    configPath,
    'config/cli.yaml',
    'config/cli.yml',
    '.qi-cli.yaml',
    '.qi-cli.yml',
  ].filter(Boolean)

  for (const path of possiblePaths) {
    if (path && existsSync(path)) {
      try {
        const fileContent = readFileSync(path, 'utf8')
        const fileConfig = YAML.load(fileContent) as Partial<CLIConfigWithFramework>

        return { ...config, ...fileConfig }
      } catch (error) {
        console.warn(`Warning: Failed to load config from ${path}:`, error)
      }
    }
  }

  return config
}

/**
 * Load configuration from environment variables
 */
function mergeConfigFromEnv(
  config: CLIConfigWithFramework,
  env: Record<string, string | undefined>
): CLIConfigWithFramework {
  const envConfig: Partial<CLIConfigWithFramework> = {}

  for (const [envVar, configKey] of Object.entries(envMappings)) {
    const envValue = env[envVar]
    if (envValue !== undefined) {
      ;(envConfig as Record<string, unknown>)[configKey] = parseEnvValue(envValue, configKey)
    }
  }

  return { ...config, ...envConfig }
}

/**
 * Load configuration from command line arguments
 */
function mergeConfigFromArgs(
  config: CLIConfigWithFramework,
  args: string[]
): CLIConfigWithFramework {
  const argConfig: Partial<CLIConfigWithFramework> = {}

  for (let i = 0; i < args.length; i++) {
    const arg = args[i]
    const mapping = argMappings[arg as keyof typeof argMappings]

    if (mapping) {
      if (typeof mapping === 'string') {
        // Argument expects a value
        const value = args[i + 1]
        if (value && !value.startsWith('-')) {
          ;(argConfig as Record<string, unknown>)[mapping] = parseArgValue(value, mapping)
          i++ // Skip the next argument as it's the value
        }
      } else if (typeof mapping === 'object') {
        // Boolean flag
        ;(argConfig as Record<string, unknown>)[mapping.key] = mapping.value
      }
    }

    // Handle boolean flags
    if (arg === '--debug' || arg === '-d') {
      argConfig.debug = true
    }
  }

  return { ...config, ...argConfig }
}

/**
 * Parse environment variable value
 */
function parseEnvValue(value: string, key: string): any {
  switch (key) {
    case 'framework':
      return value as CLIFramework

    case 'debug':
    case 'enableHotkeys':
    case 'enableModeIndicator':
    case 'enableProgressDisplay':
    case 'enableStreaming':
    case 'colors':
    case 'animations':
    case 'autoComplete':
      return value.toLowerCase() === 'true' || value === '1'

    case 'historySize':
    case 'streamingThrottle':
    case 'maxBufferSize': {
      const num = Number.parseInt(value, 10)
      return Number.isNaN(num) ? undefined : num
    }

    case 'prompt':
      return value

    default:
      return value
  }
}

/**
 * Parse command line argument value
 */
function parseArgValue(value: string, key: string): any {
  switch (key) {
    case 'framework':
      return value as CLIFramework

    case 'historySize':
    case 'streamingThrottle':
    case 'maxBufferSize': {
      const num = Number.parseInt(value, 10)
      return Number.isNaN(num) ? undefined : num
    }

    default:
      return value
  }
}

/**
 * Validate configuration
 */
function validateConfig(config: CLIConfigWithFramework): CLIConfigWithFramework {
  // Validate framework
  const validFrameworks: CLIFramework[] = ['readline', 'ink', 'hybrid']
  if (!validFrameworks.includes(config.framework)) {
    console.warn(`Warning: Invalid framework '${config.framework}', using 'readline'`)
    config.framework = 'readline'
  }

  // Validate numeric values
  if (config.historySize < 1) {
    config.historySize = 100
  }

  if (config.streamingThrottle < 0) {
    config.streamingThrottle = 10
  }

  if (config.maxBufferSize < 1000) {
    config.maxBufferSize = 1000000
  }

  return config
}

/**
 * Get available frameworks in the current environment
 */
export function getAvailableFrameworks(): CLIFramework[] {
  const frameworks: CLIFramework[] = ['readline'] // Always available

  // Check if Ink is available (defer runtime check for bundling compatibility)
  const isDevelopment = process.env.NODE_ENV !== 'production'
  if (isDevelopment) {
    frameworks.push('ink')
  }
  // In production/bundled environment, Ink availability is checked at runtime

  // Hybrid is a meta-framework that wraps Ink capabilities
  frameworks.push('hybrid')
  return frameworks
}

/**
 * Auto-detect the best framework for the current environment
 */
export function autoDetectFramework(): CLIFramework {
  const available = getAvailableFrameworks()

  // Prefer Ink if available and terminal supports colors
  if (available.includes('ink') && process.stdout.isTTY) {
    return 'ink'
  }

  // Default to readline
  return 'readline'
}

/**
 * Display configuration help
 */
export function displayConfigHelp(): void {
  console.log(`
CLI Framework Configuration

Environment Variables:
  QI_CLI_FRAMEWORK        Framework to use (readline|ink|hybrid)
  QI_CLI_DEBUG           Enable debug mode (true|false)
  QI_CLI_ENABLE_HOTKEYS  Enable hotkeys (true|false)
  QI_CLI_ENABLE_STREAMING Enable streaming (true|false)
  QI_CLI_COLORS          Enable colors (true|false)
  QI_CLI_ANIMATIONS      Enable animations (true|false)

Command Line Arguments:
  --framework, -f        Framework to use (readline|ink|hybrid)
  --debug, -d           Enable debug mode
  --no-hotkeys          Disable hotkeys
  --no-streaming        Disable streaming
  --no-colors           Disable colors
  --no-animations       Disable animations
  --prompt              Set custom prompt
  --history-size        Set history size (default: 100)

Configuration File:
  config/cli.yaml       YAML configuration file
  .qi-cli.yaml         Local YAML configuration file

Available Frameworks: ${getAvailableFrameworks().join(', ')}
Auto-detected Framework: ${autoDetectFramework()}
`)
}
