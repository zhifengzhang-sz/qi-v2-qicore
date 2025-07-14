import { defineConfig } from 'tsup'

export default defineConfig({
  entry: {
    index: 'src/index.ts',
    base: 'src/base/index.ts',
  },
  format: ['cjs', 'esm'],
  dts: true,
  sourcemap: true,
  clean: true,
  splitting: false,
  minify: true,
  target: 'es2022',
  outDir: 'dist',
  external: ['ioredis', 'pino', 'eventemitter3', '@opentelemetry/api', '@opentelemetry/sdk-node'],
})
