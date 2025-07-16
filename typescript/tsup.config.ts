import { defineConfig } from 'tsup'

export default defineConfig({
  entry: {
    index: 'lib/src/index.ts',
    base: 'lib/src/base/index.ts',
    core: 'lib/src/core/index.ts',
  },
  format: ['cjs', 'esm'],
  dts: true,
  sourcemap: true,
  clean: true,
  splitting: false,
  minify: true,
  target: 'es2023',
  outDir: 'dist',
  external: [],
})
