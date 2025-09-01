import { defineConfig } from 'tsup'

export default defineConfig({
  entry: {
    index: 'src/index.ts',
  },
  format: ['cjs', 'esm'],
  dts: false, // We handle types separately with tsc + dts-bundle-generator
  clean: true,
  sourcemap: true,
  target: 'es2023',
  external: ['react', 'ink', 'ink-text-input'],
})
