import { defineConfig } from "tsup";

export default defineConfig({
  entry: {
    index: "src/index.ts",
  },
  format: ["cjs", "esm"],
  dts: true, // Generate TypeScript declarations directly
  sourcemap: true,
  clean: true,
  splitting: false,
  minify: true,
  target: "es2023",
  outDir: "dist",
  external: ["@qi/base"], // Don't bundle @qi/base dependency
  bundle: true,
  tsconfig: "./tsconfig.json",
});
