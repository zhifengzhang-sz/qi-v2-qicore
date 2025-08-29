import { defineConfig } from 'tsup'

export default defineConfig({
  entry: ['src/index.ts'],
  format: ['cjs', 'esm'],
  dts: {
    resolve: true,
  },
  clean: true,
  splitting: false,
  sourcemap: true,
  minify: false,
  outDir: 'dist',
  target: 'es2022',
  platform: 'node',
  tsconfig: './tsconfig.json',
})
