/**
 * QiError Tests - Contract Compliance
 */

import { describe, it, expect } from "vitest";
import {
  create,
  createError,
  errorToString,
  getCategory,
  withContext,
  validationError,
  networkError,
  systemError,
  type ErrorCategory,
} from "@qi/base";

describe("QiError Factory Operations", () => {
  it("create produces valid QiError", () => {
    const error = create("TEST_ERROR", "Test message", "SYSTEM");

    expect(error.code).toBe("TEST_ERROR");
    expect(error.message).toBe("Test message");
    expect(error.category).toBe("SYSTEM");
    expect(error.context).toEqual({});
  });

  it("create with context includes context", () => {
    const context = { userId: "123", operation: "test" };
    const error = create("TEST_ERROR", "Test message", "SYSTEM", context);

    expect(error.context).toEqual(context);
  });

  it("createError from options works", () => {
    const error = createError({
      code: "TEST_ERROR",
      message: "Test message",
      category: "VALIDATION",
      context: { field: "email" },
    });

    expect(error.code).toBe("TEST_ERROR");
    expect(error.message).toBe("Test message");
    expect(error.category).toBe("VALIDATION");
    expect(error.context).toEqual({ field: "email" });
  });
});

describe("QiError Query Operations", () => {
  const error = create("TEST_ERROR", "Test message", "SYSTEM", { key: "value" });

  it("toString returns formatted string", () => {
    const str = errorToString(error);
    expect(str).toContain("TEST_ERROR");
    expect(str).toContain("Test message");
  });

  it("getCategory returns category", () => {
    expect(getCategory(error)).toBe("SYSTEM");
  });
});

describe("QiError Transformation Operations", () => {
  it("withContext adds context immutably", () => {
    const original = create("TEST_ERROR", "Test message", "SYSTEM");
    const newContext = { userId: "123" };
    const updated = withContext(newContext, original);

    expect(updated.context).toEqual(newContext);
    expect(original.context).toEqual({}); // Original unchanged
    expect(updated.code).toBe(original.code);
    expect(updated.message).toBe(original.message);
    expect(updated.category).toBe(original.category);
  });

  it("withContext merges with existing context", () => {
    const original = create("TEST_ERROR", "Test message", "SYSTEM", { a: 1 });
    const additional = { b: 2 };
    const updated = withContext(additional, original);

    expect(updated.context).toEqual({ a: 1, b: 2 });
  });
});

describe("QiError Convenience Factories", () => {
  it("validationError creates VALIDATION error", () => {
    const error = validationError("Invalid input");
    expect(error.category).toBe("VALIDATION");
    expect(error.message).toBe("Invalid input");
  });

  it("networkError creates NETWORK error", () => {
    const error = networkError("Connection failed");
    expect(error.category).toBe("NETWORK");
    expect(error.message).toBe("Connection failed");
  });

  it("systemError creates SYSTEM error", () => {
    const error = systemError("System failure");
    expect(error.category).toBe("SYSTEM");
    expect(error.message).toBe("System failure");
  });
});

describe("QiError Categories", () => {
  const categories: ErrorCategory[] = [
    "VALIDATION",
    "NETWORK",
    "SYSTEM",
    "BUSINESS",
    "AUTHENTICATION",
    "AUTHORIZATION",
    "CONFIGURATION",
    "TIMEOUT",
    "RESOURCE",
    "CONCURRENCY",
  ];

  for (const category of categories) {
    it(`${category} is valid error category`, () => {
      const error = create("TEST_ERROR", "Test message", category);
      expect(error.category).toBe(category);
    });
  }
});
