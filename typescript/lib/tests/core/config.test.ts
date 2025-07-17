/**
 * Config Tests - Contract Compliance
 * Focus: Law, Interfaces, Behavior
 */

import { describe, it, expect } from 'vitest'
import {
  ConfigBuilder,
  Config,
  fromObject,
  fromEnv,
  empty,
  validateConfig,
  safeParseConfig,
  configError,
  type ConfigData,
} from '@qi/core'
import { isSuccess, isFailure } from '@qi/base'
import { z } from 'zod'

describe('Config Factory Operations', () => {
  it('fromObject creates config from object', () => {
    const config = fromObject({ key: 'value' })
    expect(config).toBeInstanceOf(Config)
    expect(config.has('key')).toBe(true)
  })

  it('fromEnv creates config from environment', () => {
    const config = fromEnv('TEST_')
    expect(config).toBeInstanceOf(Config)
  })

  it('empty creates empty config', () => {
    const config = empty()
    expect(config).toBeDefined()
    expect(config).toBeInstanceOf(Config)
  })

  it('ConfigBuilder.fromObject creates builder', () => {
    const builder = ConfigBuilder.fromObject({ key: 'value' })
    expect(builder).toBeDefined()
    expect(typeof builder.build).toBe('function')
  })

  it('ConfigBuilder.fromEnv creates builder', () => {
    const builder = ConfigBuilder.fromEnv('TEST_')
    expect(builder).toBeDefined()
    expect(typeof builder.build).toBe('function')
  })
})

describe('Config Interface Operations', () => {
  const testData: ConfigData = {
    app: {
      name: 'test-app',
      version: '1.0.0',
    },
    database: {
      host: 'localhost',
      port: 5432,
    },
  }

  const config = fromObject(testData)

  it('config has all required methods', () => {
    expect(config).toBeDefined()
    expect(typeof config.get).toBe('function')
    expect(typeof config.getOr).toBe('function')
    expect(typeof config.has).toBe('function')
    expect(typeof config.getAll).toBe('function')
    expect(typeof config.getSources).toBe('function')
    expect(typeof config.isValidated).toBe('function')
    expect(typeof config.toBuilder).toBe('function')
    expect(typeof config.merge).toBe('function')
  })

  it('get retrieves values by path', () => {
    const nameResult = config.get('app.name')
    expect(isSuccess(nameResult)).toBe(true)
    expect(nameResult.tag === 'success' && nameResult.value).toBe('test-app')
  })

  it('get returns failure for missing keys', () => {
    const result = config.get('missing.key')
    expect(isFailure(result)).toBe(true)
  })

  it('getOr returns value or default', () => {
    const name = config.getOr('app.name', 'default')
    expect(name).toBe('test-app')

    const missing = config.getOr('missing.key', 'default')
    expect(missing).toBe('default')
  })

  it('has checks key existence', () => {
    expect(config.has('app.name')).toBe(true)
    expect(config.has('missing.key')).toBe(false)
  })

  it('getAll returns all data', () => {
    const allData = config.getAll()
    expect(allData).toEqual(testData)
  })

  it('getSources returns source list', () => {
    const sources = config.getSources()
    expect(Array.isArray(sources)).toBe(true)
    expect(sources).toContain('object')
  })

  it('isValidated returns validation status', () => {
    const validated = config.isValidated()
    expect(typeof validated).toBe('boolean')
  })
})

describe('Config Behavior - Monoid Laws', () => {
  const config1 = fromObject({ a: 1, b: { x: 1 } })
  const config2 = fromObject({ a: 2, b: { y: 2 } })
  const config3 = fromObject({ a: 3, c: 3 })
  const emptyConfig = fromObject({})

  it('merge is associative', () => {
    const left = config1.merge(config2).merge(config3)
    const right = config1.merge(config2.merge(config3))

    expect(left.getAll()).toEqual(right.getAll())
  })

  it('empty is left identity', () => {
    const merged = emptyConfig.merge(config1)
    expect(merged.getAll()).toEqual(config1.getAll())
  })

  it('empty is right identity', () => {
    const merged = config1.merge(emptyConfig)
    expect(merged.getAll()).toEqual(config1.getAll())
  })

  it('merge is right-biased (later configs override)', () => {
    const merged = config1.merge(config2)
    expect(merged.getOr('a', 0)).toBe(2) // config2 value
  })

  it('merge performs deep merge for nested objects', () => {
    const merged = config1.merge(config2)
    expect(merged.getOr('b.x', 0)).toBe(1) // from config1
    expect(merged.getOr('b.y', 0)).toBe(2) // from config2
  })
})

describe('Config Builder Behavior', () => {
  it('builder operations are immutable', () => {
    const builder1 = ConfigBuilder.fromObject({ a: 1 })
    const builder2 = builder1.mergeObject({ b: 2 })

    expect(builder1.getData()).toEqual({ a: 1 })
    expect(builder2.getData()).toEqual({ a: 1, b: 2 })
  })

  it('build creates config instance', () => {
    const builder = ConfigBuilder.fromObject({ key: 'value' })
    const result = builder.build()

    expect(isSuccess(result)).toBe(true)
    if (result.tag === 'success') {
      expect(result.value).toBeInstanceOf(Config)
    }
  })

  it('buildValidated requires validation', () => {
    const builder = ConfigBuilder.fromObject({ key: 'value' })
    const result = builder.buildValidated()

    expect(isFailure(result)).toBe(true)
  })

  it('buildValidated works after validation', () => {
    const schema = z.object({ key: z.string() })
    const builder = ConfigBuilder.fromObject({ key: 'value' }).validateWith(schema)

    const result = builder.buildValidated()
    expect(isSuccess(result)).toBe(true)
  })
})

describe('Config Validation', () => {
  const schema = z.object({
    name: z.string(),
    port: z.number(),
  })

  it('validateConfig validates against schema', () => {
    const validData = { name: 'test', port: 3000 }
    const invalidData = { name: 'test', port: 'invalid' }

    const config1 = fromObject(validData)
    const config2 = fromObject(invalidData)

    const result1 = validateConfig(config1, schema)
    expect(isSuccess(result1)).toBe(true)

    const result2 = validateConfig(config2, schema)
    expect(isFailure(result2)).toBe(true)
  })

  it('safeParseConfig parses with validation', () => {
    const validData = { name: 'test', port: 3000 }
    const invalidData = { name: 'test', port: 'invalid' }

    const result1 = safeParseConfig(validData, schema)
    const result2 = safeParseConfig(invalidData, schema)

    expect(isSuccess(result1)).toBe(true)
    expect(isFailure(result2)).toBe(true)
  })
})

describe('Config Error Factory', () => {
  it('configError creates config-specific error', () => {
    const error = configError('Config failed', { source: 'json', path: '/test' })

    expect(error.category).toBe('CONFIGURATION')
    expect(error.message).toBe('Config failed')
    expect(error.context.source).toBe('json')
    expect(error.context.path).toBe('/test')
  })

  it('configError defaults context to empty', () => {
    const error = configError('Config failed')

    expect(error.category).toBe('CONFIGURATION')
    expect(error.message).toBe('Config failed')
    expect(error.context).toEqual({})
  })
})
