#!/usr/bin/env node

import { join, dirname } from 'node:path'
import { fileURLToPath } from 'node:url'

const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)
const configDir = join(__dirname, '..', 'configs')
import { ConfigBuilder, type ValidatedConfig } from '@qi/core/config'
import { createLogger, type Logger } from '@qi/core/logger'
import { flatMapPromise, matchAsync, match } from '@qi/base'

async function main() {
  const environment = process.argv[2] || 'development'

  // ✅ CLEAN: Use matchAsync to eliminate manual Result unwrapping
  await matchAsync(
    async (logger) => {
      logger.info('✓ Logger initialized successfully!')

      // ✅ CLEAN: Use flatMapPromise to eliminate Promise<Result<T>> unwrapping anti-patterns
      const validatedConfigResult = await flatMapPromise(
        (builder) =>
          builder
            .merge(ConfigBuilder.fromEnv('APP'))
            .validateWithSchemaFile(join(configDir, 'config.schema.json'))
            .buildValidated(),
        ConfigBuilder.fromYamlFile(join(configDir, `${environment}.yaml`))
      )

      // ✅ CLEAN: Use matchAsync for clean config handling
      await matchAsync(
        async (config) => {
          await showConfigurationExample(config, logger)
        },
        async (error) => {
          logger.error('❌ Configuration failed:', undefined, {
            error: error.message,
            context: error.context,
          })
          process.exit(1)
        },
        validatedConfigResult
      )
    },
    async (error) => {
      console.error('❌ Logger initialization failed:', error.message)
      process.exit(1)
    },
    createLogger({ level: 'info', pretty: true })
  )
}

async function showConfigurationExample(config: ValidatedConfig, logger: Logger) {
  logger.info('✓ Configuration loaded and validated successfully!')

  // New v-0.3.5 API: ValidatedConfig provides direct value access
  logger.info('App Configuration (ValidatedConfig API):', {
    name: config.get('app.name'), // Direct value access, throws if missing
    port: config.get('app.port'), // Type-safe, no Result wrapper
    debug: config.getOr('app.debug', false), // Safe fallback for optional values
  })

  logger.info('Database Configuration (ValidatedConfig API):', {
    host: config.get('database.host'),
    port: config.get('database.port'),
    name: config.get('database.name'),
  })

  // ✅ For comparison: old Result-based API with functional composition (no manual unwrapping)
  const regularConfig = config.toConfig()
  match(
    (name) => logger.info('✓ Regular Config API with functional composition:', { name }),
    (error) =>
      logger.warn('⚠️ Could not get app name from regular config:', { error: error.message }),
    regularConfig.get('app.name')
  )

  logger.info('🎉 Ready to use type-safe configuration in your application!')
  logger.info('💡 ValidatedConfig eliminates Result boilerplate for validated configs')
  logger.info('💡 Async helpers eliminate Promise<Result<T>> unwrapping anti-patterns')
  logger.info('💡 Functional composition eliminates manual Result unwrapping')
  logger.info('💡 Try invalid config: APP_APP_PORT=999999 npm run dev')
}

main().catch(console.error)
