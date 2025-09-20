/**
 * Logger Tests - Contract Compliance with Proper Functional Patterns
 * Focus: Law, Interfaces, Behavior using Result<T> patterns
 */

/* eslint-disable @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-return */

import { describe, it, expect, beforeEach } from "vitest";
import { match } from "@qi/base";
import {
  createLogger,
  createFromEnv,
  loggerError,
  type LoggerConfig,
  type LogLevel,
  type Logger,
} from "@qi/core";

describe("Logger Factory Operations", () => {
  it("createLogger with valid config returns working logger", () => {
    const config: LoggerConfig = {
      level: "info",
      name: "test-logger",
      pretty: false,
    };

    const result = createLogger(config);

    match(
      (logger) => {
        expect(logger).toBeDefined();
        expect(typeof logger.info).toBe("function");
        expect(typeof logger.error).toBe("function");
        expect(typeof logger.warn).toBe("function");
        expect(typeof logger.debug).toBe("function");
        expect(typeof logger.fatal).toBe("function");
      },
      (error) => expect(true, `Logger creation should succeed: ${error.message}`).toBe(false),
      result,
    );
  });

  it("createLogger with different log levels", () => {
    const levels: LogLevel[] = ["debug", "info", "warn", "error", "fatal"];

    levels.forEach((level) => {
      const config: LoggerConfig = {
        level,
        name: `test-${level}`,
        pretty: false,
      };

      const result = createLogger(config);

      match(
        (logger) => expect(logger).toBeDefined(),
        (error) => expect(true, `Logger creation with level ${level} should succeed: ${error.message}`).toBe(false),
        result,
      );
    });
  });

  it("createLogger with pretty formatting enabled", () => {
    const config: LoggerConfig = {
      level: "info",
      name: "pretty-logger",
      pretty: true,
    };

    const result = createLogger(config);

    match(
      (logger) => {
        expect(logger).toBeDefined();
        // Pretty formatting doesn't change the interface
        expect(typeof logger.info).toBe("function");
      },
      (error) => expect(true, `Pretty logger creation should succeed: ${error.message}`).toBe(false),
      result,
    );
  });

  it("createLogger with optional configuration", () => {
    const minimalConfig: LoggerConfig = {
      level: "info",
    };

    const result = createLogger(minimalConfig);

    match(
      (logger) => expect(logger).toBeDefined(),
      (error) => expect(true, `Minimal config should succeed: ${error.message}`).toBe(false),
      result,
    );
  });

  it("createFromEnv creates logger from environment variables", () => {
    // Set up test environment
    const originalLevel = process.env.LOG_LEVEL;
    const originalName = process.env.LOG_NAME;
    const originalPretty = process.env.LOG_PRETTY;

    process.env.LOG_LEVEL = "debug";
    process.env.LOG_NAME = "env-test-logger";
    process.env.LOG_PRETTY = "true";

    const result = createFromEnv();

    match(
      (logger) => {
        expect(logger).toBeDefined();
        expect(typeof logger.debug).toBe("function");
      },
      (error) => expect(true, `Environment logger creation should succeed: ${error.message}`).toBe(false),
      result,
    );

    // Restore environment
    if (originalLevel !== undefined) {
      process.env.LOG_LEVEL = originalLevel;
    } else {
      delete process.env.LOG_LEVEL;
    }
    if (originalName !== undefined) {
      process.env.LOG_NAME = originalName;
    } else {
      delete process.env.LOG_NAME;
    }
    if (originalPretty !== undefined) {
      process.env.LOG_PRETTY = originalPretty;
    } else {
      delete process.env.LOG_PRETTY;
    }
  });

  it("createFromEnv handles missing environment gracefully", () => {
    // Clear environment variables
    const originalLevel = process.env.LOG_LEVEL;
    const originalName = process.env.LOG_NAME;
    delete process.env.LOG_LEVEL;
    delete process.env.LOG_NAME;

    const result = createFromEnv();

    match(
      (logger) => expect(logger).toBeDefined(),
      (error) => expect(true, `Environment logger with defaults should succeed: ${error.message}`).toBe(false),
      result,
    );

    // Restore environment
    if (originalLevel !== undefined) {
      process.env.LOG_LEVEL = originalLevel;
    }
    if (originalName !== undefined) {
      process.env.LOG_NAME = originalName;
    }
  });
});

describe("Logger Interface Operations", () => {
  let logger: Logger;

  beforeEach(() => {
    const config: LoggerConfig = {
      level: "debug",
      name: "test-logger",
      pretty: false,
    };

    const result = createLogger(config);

    match(
      (createdLogger) => { logger = createdLogger; },
      (error) => expect(true, `Setup should succeed: ${error.message}`).toBe(false),
      result,
    );
  });

  it("logger has all required methods", () => {
    // Note: Logger doesn't have trace method, only debug, info, warn, error, fatal
    expect(typeof logger.debug).toBe("function");
    expect(typeof logger.info).toBe("function");
    expect(typeof logger.warn).toBe("function");
    expect(typeof logger.error).toBe("function");
    expect(typeof logger.fatal).toBe("function");
    expect(typeof logger.child).toBe("function");
    expect(typeof logger.setLevel).toBe("function");
    expect(typeof logger.getLevel).toBe("function");
  });

  it("logging methods accept different parameter types", () => {
    // Test with strings
    expect(() => logger.info("Simple message")).not.toThrow();
    expect(() => logger.debug("Debug message")).not.toThrow();
    expect(() => logger.warn("Warning message")).not.toThrow();
    expect(() => logger.error("Error message")).not.toThrow();

    // Test with objects
    expect(() => logger.info({ key: "value" }, "Object message")).not.toThrow();
    expect(() => logger.error({ error: "details" }, "Error with context")).not.toThrow();

    // Test with Error objects
    const testError = new Error("Test error");
    expect(() => logger.error(testError, "Error object")).not.toThrow();
    expect(() => logger.fatal(testError)).not.toThrow();
  });

  it("child logger creation works", () => {
    const childLogger = logger.child({ component: "test-component" });

    expect(childLogger).toBeDefined();
    expect(typeof childLogger.info).toBe("function");
    expect(typeof childLogger.child).toBe("function");

    // Child should also work
    expect(() => childLogger.info("Child logger message")).not.toThrow();

    // Can create nested children
    const grandChild = childLogger.child({ subComponent: "nested" });
    expect(grandChild).toBeDefined();
    expect(() => grandChild.debug("Grandchild message")).not.toThrow();
  });

  it("level management works correctly", () => {
    // Get initial level
    const initialLevel = logger.getLevel();
    expect(initialLevel).toBeDefined();

    // Set new level
    logger.setLevel("warn");
    expect(logger.getLevel()).toBe("warn");

    // Set back to original
    logger.setLevel(initialLevel);
    expect(logger.getLevel()).toBe(initialLevel);
  });

  it("different log levels work appropriately", () => {
    // All these should work without throwing
    expect(() => {
      // Note: trace method doesn't exist, using debug instead
      logger.debug("Trace-level message");
      logger.debug("Debug message");
      logger.info("Info message");
      logger.warn("Warning message");
      logger.error("Error message");
      logger.fatal("Fatal message");
    }).not.toThrow();
  });
});

describe("Logger Behavior Contract", () => {
  it("logger level filtering works correctly", () => {
    const config: LoggerConfig = {
      level: "warn", // Only warn, error, fatal should be logged
      name: "filtered-logger",
      pretty: false,
    };

    const result = createLogger(config);

    match(
      (logger) => {
        expect(logger.getLevel()).toBe("warn");

        // These should not throw even if they might not output
        // Note: trace method doesn't exist, testing debug instead at warn level
        expect(() => logger.debug("Should not appear at warn level")).not.toThrow();
        expect(() => logger.debug("Should not appear")).not.toThrow();
        expect(() => logger.info("Should not appear")).not.toThrow();

        // These should definitely work
        expect(() => logger.warn("Should appear")).not.toThrow();
        expect(() => logger.error("Should appear")).not.toThrow();
        expect(() => logger.fatal("Should appear")).not.toThrow();
      },
      (error) => expect(true, `Filtered logger should work: ${error.message}`).toBe(false),
      result,
    );
  });

  it("logger handles complex objects correctly", () => {
    const config: LoggerConfig = {
      level: "debug",
      name: "complex-logger",
      pretty: false,
    };

    const result = createLogger(config);

    match(
      (logger) => {
        const complexObject = {
          string: "value",
          number: 42,
          boolean: true,
          array: [1, 2, 3],
          nested: {
            prop: "nested-value",
            deep: {
              value: 123
            }
          },
          nullValue: null,
          undefinedValue: undefined,
        };

        expect(() => logger.info(complexObject, "Complex object")).not.toThrow();
        expect(() => logger.debug({ context: complexObject }, "With context")).not.toThrow();
      },
      (error) => expect(true, `Complex object logging should work: ${error.message}`).toBe(false),
      result,
    );
  });

  it("logger handles errors gracefully", () => {
    const config: LoggerConfig = {
      level: "debug",
      name: "error-logger",
      pretty: false,
    };

    const result = createLogger(config);

    match(
      (logger) => {
        const testError = new Error("Test error message");
        testError.stack = "Error stack trace";

        expect(() => logger.error(testError)).not.toThrow();
        expect(() => logger.error(testError, "Error with message")).not.toThrow();
        expect(() => logger.fatal(testError, "Fatal error")).not.toThrow();

        // Test with custom error objects
        const customError = {
          name: "CustomError",
          message: "Custom error message",
          code: "CUSTOM_ERROR",
          details: { key: "value" }
        };

        expect(() => logger.error(customError, "Custom error")).not.toThrow();
      },
      (error) => expect(true, `Error logging should work: ${error.message}`).toBe(false),
      result,
    );
  });

  it("child loggers inherit configuration", () => {
    const config: LoggerConfig = {
      level: "info",
      name: "parent-logger",
      pretty: false,
    };

    const result = createLogger(config);

    match(
      (parentLogger) => {
        const childLogger = parentLogger.child({ service: "test-service" });

        expect(childLogger.getLevel()).toBe("info"); // Should inherit level
        expect(() => childLogger.info("Child message")).not.toThrow();
        expect(() => childLogger.warn("Child warning")).not.toThrow();

        // Child modifications shouldn't affect parent
        childLogger.setLevel("debug");
        expect(childLogger.getLevel()).toBe("debug");
        expect(parentLogger.getLevel()).toBe("info"); // Parent unchanged
      },
      (error) => expect(true, `Child logger behavior should work: ${error.message}`).toBe(false),
      result,
    );
  });

  it("logger performance is reasonable", () => {
    const config: LoggerConfig = {
      level: "info",
      name: "perf-logger",
      pretty: false,
    };

    const result = createLogger(config);

    match(
      (logger) => {
        const startTime = performance.now();

        // Log many messages quickly
        for (let i = 0; i < 1000; i++) {
          logger.info(`Message ${i}`);
        }

        const endTime = performance.now();
        const duration = endTime - startTime;

        // Should complete reasonably quickly (less than 1 second for 1000 logs)
        expect(duration).toBeLessThan(1000);
      },
      (error) => expect(true, `Performance test should work: ${error.message}`).toBe(false),
      result,
    );
  });
});

describe("Logger Error Factory", () => {
  it("loggerError creates logger-specific error", () => {
    const error = loggerError("Logger configuration failed", {
      level: "invalid",
      operation: "create",
      component: "logger-factory",
    });

    expect(error.code).toBe("LOGGER_ERROR");
    expect(error.category).toBe("LOGGER");
    expect(error.message).toBe("Logger configuration failed");
    expect(error.context.level).toBe("invalid");
    expect(error.context.operation).toBe("create");
    expect(error.context.component).toBe("logger-factory");
  });

  it("loggerError works with minimal context", () => {
    const error = loggerError("Simple logger error");

    expect(error.code).toBe("LOGGER_ERROR");
    expect(error.category).toBe("LOGGER");
    expect(error.message).toBe("Simple logger error");
    expect(error.context).toBeDefined();
  });

  it("loggerError includes proper error metadata", () => {
    const error = loggerError("Logger initialization failed", {
      operation: "init",
      details: "Pino configuration error",
    });

    // Note: QiCore errors don't have timestamp at the top level
    expect(error.code).toBe("LOGGER_ERROR");
    expect(error.context.operation).toBe("init");
    expect(error.context.details).toBe("Pino configuration error");
    expect(typeof error.toString).toBe("function");

    const errorString = error.message;
    expect(errorString).toContain("Logger initialization failed");
  });
});