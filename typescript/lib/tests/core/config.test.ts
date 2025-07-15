/**
 * Unit tests for Config module
 */

import { mkdir, unlink, writeFile } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import {
  AppConfigSchema,
  Config,
  ConfigBuilder,
  configError,
  empty,
  fromEnv,
  fromJsonFile,
  fromObject,
  safeParseConfig,
  validateConfig,
} from '@qi/core'
import { afterEach, beforeEach, describe, expect, test } from 'vitest'
import { z } from 'zod'

describe('Config Module', () => {
  let tempDir: string
  let testFiles: string[] = []

  beforeEach(async () => {
    tempDir = join(tmpdir(), 'qi-config-test')
    await mkdir(tempDir, { recursive: true })
    testFiles = []
  })

  afterEach(async () => {
    // Clean up test files
    for (const file of testFiles) {
      try {
        await unlink(file)
      } catch {}
    }
  })

  const createTestFile = async (filename: string, content: string): Promise<string> => {
    const filepath = join(tempDir, filename)
    await writeFile(filepath, content)
    testFiles.push(filepath)
    return filepath
  }

  describe('ConfigBuilder', () => {
    test('fromObject creates builder with data', () => {
      const data = { key: 'value', nested: { prop: 123 } }
      const builder = ConfigBuilder.fromObject(data)

      expect(builder.getData()).toEqual(data)
      expect(builder.getSources()).toEqual(['object'])
    })

    test('fromJsonFile loads JSON configuration', async () => {
      const data = { app: { name: 'test' }, port: 3000 }
      const filepath = await createTestFile('config.json', JSON.stringify(data))

      const result = await ConfigBuilder.fromJsonFile(filepath)

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        expect(result.value.getData()).toEqual(data)
        expect(result.value.getSources()).toEqual(['json'])
      }
    })

    test('fromYamlFile loads YAML configuration', async () => {
      const yaml = `
app:
  name: test
  port: 3000
database:
  host: localhost
`
      const filepath = await createTestFile('config.yaml', yaml)

      const result = await ConfigBuilder.fromYamlFile(filepath)

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        const data = result.value.getData()
        expect(data.app).toEqual({ name: 'test', port: 3000 })
        expect(data.database).toEqual({ host: 'localhost' })
      }
    })

    test('fromTomlFile loads TOML configuration', async () => {
      const toml = `
[app]
name = "test"
port = 3000

[database]
host = "localhost"
`
      const filepath = await createTestFile('config.toml', toml)

      const result = await ConfigBuilder.fromTomlFile(filepath)

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        const data = result.value.getData()
        expect(data.app).toEqual({ name: 'test', port: 3000 })
        expect(data.database).toEqual({ host: 'localhost' })
      }
    })

    test('fromEnv loads environment variables', () => {
      // Set test environment variables
      process.env.TEST_APP_NAME = 'test-app'
      process.env.TEST_PORT = '3000'
      process.env.OTHER_VAR = 'should-not-include'

      const builder = ConfigBuilder.fromEnv('TEST')
      const data = builder.getData()

      expect(data.app_name).toBe('test-app')
      expect(data.port).toBe('3000')
      expect(data.other_var).toBeUndefined()

      // Clean up
      delete process.env.TEST_APP_NAME
      delete process.env.TEST_PORT
      delete process.env.OTHER_VAR
    })

    test('merge combines configurations', () => {
      const builder1 = ConfigBuilder.fromObject({ a: 1, b: 2 })
      const builder2 = ConfigBuilder.fromObject({ b: 3, c: 4 })

      const merged = builder1.merge(builder2)

      expect(merged.getData()).toEqual({ a: 1, b: 3, c: 4 })
      expect(merged.getSources()).toEqual(['object', 'object'])
    })

    test('mergeObject combines with plain object', () => {
      const builder = ConfigBuilder.fromObject({ a: 1, b: 2 })
      const merged = builder.mergeObject({ b: 3, c: 4 })

      expect(merged.getData()).toEqual({ a: 1, b: 3, c: 4 })
    })

    test('set updates nested values', () => {
      const builder = ConfigBuilder.fromObject({})
      const updated = builder.set('app.name', 'test').set('app.port', 3000)

      expect(updated.getData()).toEqual({
        app: { name: 'test', port: 3000 },
      })
    })

    test('transform applies function to data', () => {
      const builder = ConfigBuilder.fromObject({ port: '3000' })
      const transformed = builder.transform((data) => ({
        ...data,
        port: Number.parseInt(data.port as string),
      }))

      expect(transformed.getData().port).toBe(3000)
    })

    test('filter removes unwanted keys', () => {
      const builder = ConfigBuilder.fromObject({
        keep: 'this',
        remove: 'this',
        also_keep: 'this',
      })

      const filtered = builder.filter((key) => key !== 'remove')

      expect(filtered.getData()).toEqual({
        keep: 'this',
        also_keep: 'this',
      })
    })

    test('validateWith applies schema validation', () => {
      const schema = z.object({
        name: z.string(),
        port: z.number(),
      })

      const builder = ConfigBuilder.fromObject({ name: 'test', port: 3000 })
      const validated = builder.validateWith(schema)

      const result = validated.build()
      expect(result.tag).toBe('success')
    })

    test('build with invalid schema returns error', () => {
      const schema = z.object({
        name: z.string(),
        port: z.number(),
      })

      const builder = ConfigBuilder.fromObject({ name: 'test', port: 'invalid' })
      const validated = builder.validateWith(schema)

      const result = validated.build()
      expect(result.tag).toBe('failure')
      if (result.tag === 'failure') {
        expect(result.error.category).toBe('CONFIGURATION')
      }
    })

    test('buildUnsafe always returns Config', () => {
      const builder = ConfigBuilder.fromObject({ key: 'value' })
      const config = builder.buildUnsafe()

      expect(config).toBeInstanceOf(Config)
      expect(config.get('key').tag).toBe('success')
    })
  })

  describe('Config', () => {
    test('get retrieves nested values', () => {
      const config = fromObject({ app: { name: 'test', port: 3000 } })

      const name = config.get('app.name')
      const port = config.get('app.port')

      expect(name.tag).toBe('success')
      expect(port.tag).toBe('success')
      if (name.tag === 'success' && port.tag === 'success') {
        expect(name.value).toBe('test')
        expect(port.value).toBe(3000)
      }
    })

    test('get returns error for missing paths', () => {
      const config = fromObject({ app: { name: 'test' } })

      const result = config.get('app.missing')

      expect(result.tag).toBe('failure')
      if (result.tag === 'failure') {
        expect(result.error.category).toBe('CONFIGURATION')
      }
    })

    test('getOr returns default for missing paths', () => {
      const config = fromObject({ app: { name: 'test' } })

      const port = config.getOr('app.port', 8080)

      expect(port).toBe(8080)
    })

    test('has checks path existence', () => {
      const config = fromObject({ app: { name: 'test' } })

      expect(config.has('app.name')).toBe(true)
      expect(config.has('app.port')).toBe(false)
    })

    test('getAll returns complete configuration', () => {
      const data = { app: { name: 'test' }, port: 3000 }
      const config = fromObject(data)

      expect(config.getAll()).toEqual(data)
    })

    test('toBuilder creates builder with same data', () => {
      const config = fromObject({ key: 'value' })
      const builder = config.toBuilder()

      expect(builder.getData()).toEqual({ key: 'value' })
    })

    test('merge combines configurations immutably', () => {
      const config1 = fromObject({ a: 1, b: 2 })
      const config2 = fromObject({ b: 3, c: 4 })

      const merged = config1.merge(config2)

      expect(merged.getAll()).toEqual({ a: 1, b: 3, c: 4 })
      expect(config1.getAll()).toEqual({ a: 1, b: 2 }) // Original unchanged
    })

    test('toJson serializes to JSON string', () => {
      const config = fromObject({ app: { name: 'test' } })
      const json = config.toJson()

      expect(JSON.parse(json)).toEqual({ app: { name: 'test' } })
    })

    test('toObject returns plain object', () => {
      const data = { app: { name: 'test' } }
      const config = fromObject(data)

      expect(config.toObject()).toEqual(data)
    })
  })

  describe('Factory Functions', () => {
    test('fromObject creates config from plain object', () => {
      const data = { key: 'value' }
      const config = fromObject(data)

      expect(config.get('key').tag).toBe('success')
    })

    test('fromEnv creates config from environment variables', () => {
      process.env.TEST_KEY = 'value'

      const config = fromEnv('TEST')

      expect(config.get('key').tag).toBe('success')

      delete process.env.TEST_KEY
    })

    test('empty creates empty configuration', () => {
      const config = empty()

      expect(config.getAll()).toEqual({})
    })
  })

  describe('Validation Utilities', () => {
    test('validateConfig validates against schema', () => {
      const schema = z.object({
        name: z.string(),
        port: z.number(),
      })

      const config = fromObject({ name: 'test', port: 3000 })
      const result = validateConfig(config, schema)

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        expect(result.value.name).toBe('test')
        expect(result.value.port).toBe(3000)
      }
    })

    test('validateConfig returns error for invalid data', () => {
      const schema = z.object({
        name: z.string(),
        port: z.number(),
      })

      const config = fromObject({ name: 'test', port: 'invalid' })
      const result = validateConfig(config, schema)

      expect(result.tag).toBe('failure')
    })

    test('safeParseConfig parses data with schema', () => {
      const schema = z.object({
        name: z.string(),
        port: z.coerce.number(),
      })

      const result = safeParseConfig({ name: 'test', port: '3000' }, schema)

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        expect(result.value.port).toBe(3000)
      }
    })
  })

  describe('Error Handling', () => {
    test('configError creates proper error structure', () => {
      const error = configError('Test error', { source: 'json', path: 'test.json' })

      expect(error.category).toBe('CONFIGURATION')
      expect(error.message).toBe('Test error')
      expect(error.context.source).toBe('json')
      expect(error.context.path).toBe('test.json')
    })

    test('file loading handles missing files', async () => {
      const result = await fromJsonFile('/nonexistent/file.json')

      expect(result.tag).toBe('failure')
      if (result.tag === 'failure') {
        expect(result.error.category).toBe('CONFIGURATION')
      }
    })

    test('file loading handles invalid JSON', async () => {
      const filepath = await createTestFile('invalid.json', '{ invalid json }')

      const result = await fromJsonFile(filepath)

      expect(result.tag).toBe('failure')
    })
  })

  describe('AppConfigSchema', () => {
    test('validates complete app configuration', () => {
      const data = {
        app: {
          name: 'test-app',
          version: '1.0.0',
          environment: 'development',
          port: 3000,
          host: 'localhost',
        },
        database: {
          host: 'db.example.com',
          port: 5432,
          name: 'mydb',
          user: 'admin',
          password: 'secret',
        },
      }

      const result = AppConfigSchema.safeParse(data)

      expect(result.success).toBe(true)
      if (result.success) {
        expect(result.data.app.name).toBe('test-app')
        expect(result.data.database?.host).toBe('db.example.com')
      }
    })

    test('applies default values', () => {
      const data = {
        app: {
          name: 'test-app',
          version: '1.0.0',
        },
      }

      const result = AppConfigSchema.safeParse(data)

      expect(result.success).toBe(true)
      if (result.success) {
        expect(result.data.app.environment).toBe('development')
        expect(result.data.app.port).toBe(3000)
        expect(result.data.app.host).toBe('localhost')
      }
    })

    test('coerces string numbers', () => {
      const data = {
        app: {
          name: 'test-app',
          version: '1.0.0',
          port: '8080',
        },
      }

      const result = AppConfigSchema.safeParse(data)

      expect(result.success).toBe(true)
      if (result.success) {
        expect(result.data.app.port).toBe(8080)
      }
    })

    test('validates required fields', () => {
      const data = {
        app: {
          version: '1.0.0', // Missing required 'name' field
        },
      }

      const result = AppConfigSchema.safeParse(data)

      expect(result.success).toBe(false)
    })
  })

  describe('Fluent API Chain', () => {
    test('chains multiple operations', () => {
      const result = ConfigBuilder.fromObject({ a: 1 })
        .mergeObject({ b: 2 })
        .set('c', 3)
        .transform((data) => ({ ...data, d: 4 }))
        .filter((key) => key !== 'a')
        .buildUnsafe()

      expect(result.getAll()).toEqual({ b: 2, c: 3, d: 4 })
    })

    test('chains with validation', () => {
      const schema = z.object({
        name: z.string(),
        port: z.coerce.number(),
      })

      const result = ConfigBuilder.fromObject({ name: 'test' })
        .set('port', '3000')
        .validateWith(schema)
        .build()

      expect(result.tag).toBe('success')
      if (result.tag === 'success') {
        expect(result.value.get('port').tag).toBe('success')
      }
    })
  })
})
