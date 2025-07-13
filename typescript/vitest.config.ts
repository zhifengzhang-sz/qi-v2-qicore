/// <reference types="vitest" />
import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    environment: "node",
    globals: true,
    include: ["tests/**/*.{test,spec}.ts"],
    exclude: ["tests/**/*.bench.ts", "node_modules/**", "dist/**"],
    coverage: {
      provider: "v8",
      reporter: ["text", "json", "html"],
      include: ["src/**/*.ts"],
      exclude: ["src/**/*.test.ts", "src/**/*.spec.ts"],
      thresholds: {
        global: {
          branches: 80,
          functions: 80,
          lines: 80,
          statements: 80,
        },
      },
    },
    typecheck: {
      enabled: true,
      tsconfig: "./tsconfig.json",
    },
    benchmark: {
      include: ["tests/**/*.bench.ts"],
    },
  },
  resolve: {
    alias: {
      "@": new URL("./src", import.meta.url).pathname,
      "@/base": new URL("./src/base", import.meta.url).pathname,
      "@/core": new URL("./src/core", import.meta.url).pathname,
      "@/tests": new URL("./tests", import.meta.url).pathname,
    },
  },
});