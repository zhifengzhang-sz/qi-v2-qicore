import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    include: ['../tests/core/**/*.test.ts'],
    exclude: ['../tests/base/**', '../tests/properties/**', '../tests/integration/**'],
    globals: true,
    environment: 'node',
  },
})