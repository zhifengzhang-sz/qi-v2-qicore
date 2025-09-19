import js from "@eslint/js";
import tseslint from "typescript-eslint";
import qiPlugin from "@qi/eslint-plugin";

export default [
  js.configs.recommended,
  ...tseslint.configs.recommendedTypeChecked,
  {
    languageOptions: {
      parserOptions: {
        projectService: true,
        tsconfigRootDir: import.meta.dirname,
      },
    },
  },
  // Apply QI rules to TypeScript files
  {
    files: ["**/*.ts", "**/*.tsx"],
    plugins: {
      "@qi": qiPlugin,
    },
    rules: {
      // Use recommended config with additional rules
      ...qiPlugin.configs.recommended.rules,
    },
  },
  {
    ignores: [
      "dist/**",
      "node_modules/**",
      "coverage/**",
      "temp/**",
      "eslint-rules/**",
      "biome-plugins/**",
    ],
  },
];
