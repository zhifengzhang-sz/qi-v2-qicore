import { defineConfig } from 'tsup'

export default defineConfig({
  entry: {
    index: 'src/index.ts',
  },
  format: ['cjs', 'esm'],
  dts: false, // Using dts-bundle-generator instead
  sourcemap: true,
  clean: true,
  splitting: false,
  minify: true,
  target: 'es2023',
  outDir: 'dist',
  external: ['react', 'ink', 'ink-text-input', 'chalk', 'xstate'],
  bundle: true,
  tsconfig: './tsconfig.json',
})
