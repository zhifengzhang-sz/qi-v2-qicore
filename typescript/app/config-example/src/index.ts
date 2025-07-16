#!/usr/bin/env node

import { join } from 'node:path'
import { ConfigBuilder } from '@qi/core/config'
import { createLogger } from '@qi/core/logger'

async function main() {
  const loggerResult = createLogger({ level: 'info', pretty: true })
  if (loggerResult.tag === 'failure') throw new Error('Logger failed')
  const logger = loggerResult.value
  const environment = process.argv[2] || 'development'

  // Load config -> merge env vars -> validate against external schema -> get ValidatedConfig
  const validatedConfigResult = await ConfigBuilder.fromYamlFile(
    join('configs', `${environment}.yaml`)
  ).then((result) =>
    result.tag === 'success'
      ? result.value
          .merge(ConfigBuilder.fromEnv('APP'))
          .validateWithSchemaFile(join('configs', 'config.schema.json'))
          .buildValidated()
      : result
  )

  if (validatedConfigResult.tag === 'failure') {
    logger.error('❌ Configuration failed:', undefined, {
      error: validatedConfigResult.error.message,
      context: validatedConfigResult.error.context,
    })
    process.exit(1)
  }

  const config = validatedConfigResult.value

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

  // For comparison: old Result-based API still available via .toConfig()
  const regularConfig = config.toConfig()
  const appNameResult = regularConfig.get<string>('app.name')
  if (appNameResult.tag === 'success') {
    logger.info('✓ Regular Config API still works:', { name: appNameResult.value })
  }

  logger.info('🎉 Ready to use type-safe configuration in your application!')
  logger.info('💡 ValidatedConfig eliminates Result boilerplate for validated configs')
  logger.info('💡 Try invalid config: APP_APP_PORT=999999 npm run dev')
}

main().catch(console.error)
