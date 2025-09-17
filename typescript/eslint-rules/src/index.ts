/**
 * @qi/eslint-plugin - ESLint rules for QiCore patterns
 *
 * Enforces proper usage of Result<T> and prevents anti-patterns
 */

import { noResultAntiPatterns } from "./no-result-anti-patterns.js";
import { noThrowInDsl } from "./no-throw-in-dsl.js";
import { enforceReadonlyDsl } from "./enforce-readonly-dsl.js";
import { noImperativeTryCatch } from "./no-imperative-try-catch.js";

const plugin = {
  meta: {
    name: "@qi/eslint-plugin",
    version: "1.1.0",
  },
  rules: {
    "no-result-anti-patterns": noResultAntiPatterns,
    "no-throw-in-dsl": noThrowInDsl,
    "enforce-readonly-dsl": enforceReadonlyDsl,
    "no-imperative-try-catch": noImperativeTryCatch,
  },
  configs: {
    recommended: {
      plugins: ["@qi"],
      rules: {
        "@qi/no-result-anti-patterns": "error",
      },
    },
    strict: {
      plugins: ["@qi"],
      rules: {
        "@qi/no-result-anti-patterns": "error",
        "@qi/no-imperative-try-catch": "error",
      },
    },
    "dsl-strict": {
      plugins: ["@qi"],
      rules: {
        "@qi/no-result-anti-patterns": "error",
        "@qi/no-throw-in-dsl": "error",
        "@qi/enforce-readonly-dsl": "error",
        "@qi/no-imperative-try-catch": [
          "error",
          {
            enforcePaths: ["**/lib/dsl/**"],
            allowTestFiles: true,
          },
        ],
      },
    },
    "actor-strict": {
      plugins: ["@qi"],
      rules: {
        "@qi/no-result-anti-patterns": "error",
        "@qi/no-imperative-try-catch": [
          "error",
          {
            enforcePaths: ["**/lib/actors/**"],
            allowTestFiles: true,
          },
        ],
      },
    },
    "full-strict": {
      plugins: ["@qi"],
      rules: {
        "@qi/no-result-anti-patterns": "error",
        "@qi/no-throw-in-dsl": "error",
        "@qi/enforce-readonly-dsl": "error",
        "@qi/no-imperative-try-catch": [
          "error",
          {
            enforcePaths: ["**/lib/dsl/**", "**/lib/actors/**", "**/lib/utils/**"],
            allowTestFiles: true,
          },
        ],
      },
    },
  },
};

export default plugin;
