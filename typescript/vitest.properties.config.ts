import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    setupFiles: ['./tests/setup.ts'],
    include: ['tests/properties/**/*.test.ts'],
    testTimeout: 30000, // Longer timeout for property tests
    pool: 'threads',
  },
  resolve: {
    alias: {
      '@': new URL('./src', import.meta.url).pathname,
      '@/base': new URL('./src/base', import.meta.url).pathname,
      '@/core': new URL('./src/core', import.meta.url).pathname,
    },
  },
})
