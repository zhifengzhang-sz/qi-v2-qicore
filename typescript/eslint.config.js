import js from '@eslint/js'
import tseslint from 'typescript-eslint'
import qiPlugin from './eslint-rules/dist/index.js'

export default [
  js.configs.recommended,
  ...tseslint.configs.recommendedTypeChecked,
  {
    languageOptions: {
      parserOptions: {
        projectService: true,
        tsconfigRootDir: import.meta.dirname,
      }
    }
  },
  {
    files: ['**/*.ts', '**/*.tsx'],
    plugins: {
      '@qi': qiPlugin
    },
    rules: {
      '@qi/no-result-anti-patterns': 'error'
    }
  },
  {
    ignores: [
      'dist/**',
      'node_modules/**',
      'coverage/**',
      'temp/**',
      'eslint-rules/**',
      'biome-plugins/**'
    ]
  }
]