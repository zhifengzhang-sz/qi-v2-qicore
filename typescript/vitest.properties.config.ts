import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    setupFiles: ['./lib/tests/setup.ts'],
    include: ['lib/tests/properties/**/*.test.ts'],
    testTimeout: 30000, // Longer timeout for property tests
    pool: 'threads',
  },
  resolve: {
    alias: {
      '@qi/base': new URL('./lib/src/base/index.ts', import.meta.url).pathname,
      '@qi/core': new URL('./lib/src/core/index.ts', import.meta.url).pathname,
    },
  },
})
