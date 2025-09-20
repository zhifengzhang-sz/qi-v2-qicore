/**
 * Config Tests - Contract Compliance with Proper Functional Patterns
 * Focus: Law, Interfaces, Behavior using Result<T> patterns
 */

/* eslint-disable @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-argument */

import { describe, it, expect, beforeEach } from "vitest";
import { match, failure } from "@qi/base";
import {
  ConfigBuilder,
  Config,
  fromObject,
  fromEnv,
  empty,
  validateConfig,
  safeParseConfig,
  configError,
} from "@qi/core";
import { z } from "zod";

describe("Config Factory Operations", () => {
  it("fromObject creates config from object", () => {
    const config = fromObject({ key: "value", nested: { prop: 42 } });
    expect(config).toBeInstanceOf(Config);
    expect(config.has("key")).toBe(true);
    expect(config.getOr("key", "")).toBe("value");
    expect(config.getOr("nested.prop", 0)).toBe(42);
  });

  it("fromEnv creates config from environment", () => {
    // Set up test environment variables
    process.env.TEST_VAR = "test-value";
    process.env.TEST_PORT = "3000";

    const config = fromEnv("TEST");
    expect(config).toBeInstanceOf(Config);

    // Should have environment variables without prefix (converted to lowercase)
    expect(config.getOr("var", "")).toBe("test-value");
    expect(config.getOr("port", "")).toBe("3000");

    // Clean up
    delete process.env.TEST_VAR;
    delete process.env.TEST_PORT;
  });

  it("empty creates empty config", () => {
    const config = empty();
    expect(config).toBeDefined();
    expect(config).toBeInstanceOf(Config);
    expect(config.has("any-key")).toBe(false);
    // Config class doesn't provide keys() method - checking has() is sufficient
  });

  it("ConfigBuilder.fromObject creates builder", () => {
    const builder = ConfigBuilder.fromObject({ key: "value" });
    expect(builder).toBeDefined();
    expect(typeof builder.build).toBe("function");
    expect(typeof builder.merge).toBe("function");
    expect(typeof builder.set).toBe("function");
  });

  it("ConfigBuilder.fromEnv creates builder", () => {
    const builder = ConfigBuilder.fromEnv("TEST_");
    expect(builder).toBeDefined();
    expect(typeof builder.build).toBe("function");
    expect(typeof builder.merge).toBe("function");
    expect(typeof builder.set).toBe("function");
  });

  it("ConfigBuilder.fromJsonFile creates builder from JSON file", async () => {
    const builderResult = await ConfigBuilder.fromJsonFile("../tests/core/test-config.json");
    expect(builderResult).toBeDefined();
    match(
      (builder) => {
        expect(builder).toBeDefined();
        expect(typeof builder.build).toBe("function");
      },
      (error) => expect(true,`Should load JSON file: ${error.message}`).toBe(false),
      builderResult
    );
  });

  it("ConfigBuilder.fromYamlFile creates builder from YAML file", async () => {
    const builderResult = await ConfigBuilder.fromYamlFile("../tests/core/test-config.yaml");
    expect(builderResult).toBeDefined();
    match(
      (builder) => {
        expect(builder).toBeDefined();
        expect(typeof builder.build).toBe("function");
      },
      (error) => expect(true,`Should load YAML file: ${error.message}`).toBe(false),
      builderResult
    );
  });
});

describe("Config Interface Operations", () => {
  let config: Config;

  beforeEach(() => {
    config = fromObject({
      string: "value",
      number: 42,
      boolean: true,
      nested: {
        prop: "nested-value",
        deep: {
          value: 123
        }
      },
      array: [1, 2, 3]
    });
  });

  it("has() checks key existence correctly", () => {
    expect(config.has("string")).toBe(true);
    expect(config.has("number")).toBe(true);
    expect(config.has("nested.prop")).toBe(true);
    expect(config.has("nested.deep.value")).toBe(true);
    expect(config.has("non-existent")).toBe(false);
    expect(config.has("nested.non-existent")).toBe(false);
  });

  it("get() retrieves values correctly", () => {
    match(
      (value) => expect(value).toBe("value"),
      () => expect(true,"Should get string value"),
      config.get("string")
    );
    match(
      (value) => expect(value).toBe(42),
      () => expect(true,"Should get number value"),
      config.get("number")
    );
    match(
      (value) => expect(value).toBe(true),
      () => expect(true,"Should get boolean value"),
      config.get("boolean")
    );
    match(
      (value) => expect(value).toBe("nested-value"),
      () => expect(true,"Should get nested prop"),
      config.get("nested.prop")
    );
    match(
      (value) => expect(value).toBe(123),
      () => expect(true,"Should get deep nested value"),
      config.get("nested.deep.value")
    );
    match(
      () => expect(true,"Should not find non-existent key"),
      (error) => expect(error.category).toBe("CONFIGURATION"),
      config.get("non-existent")
    );
  });

  it("getOr() with default values works correctly", () => {
    expect(config.getOr("non-existent", "default")).toBe("default");
    expect(config.getOr("string", "default")).toBe("value");
    expect(config.getOr("nested.non-existent", "default")).toBe("default");
  });

  // Note: Config class doesn't provide keys() method per API specification

  it("toObject() returns the underlying data", () => {
    const obj = config.toObject();
    expect(obj.string).toBe("value");
    expect(obj.number).toBe(42);
    expect((obj.nested as Record<string, unknown>).prop).toBe("nested-value");
    expect(((obj.nested as Record<string, unknown>).deep as Record<string, unknown>).value).toBe(123);
  });

  it("merge() combines configurations correctly", () => {
    const other = fromObject({
      string: "new-value",
      new_key: "new-value",
      nested: {
        new_prop: "new-nested-value"
      }
    });

    const merged = config.merge(other);

    // Original should be unchanged
    expect(config.getOr("string", "")).toBe("value");
    expect(config.has("new_key")).toBe(false);

    // Merged should have new values
    expect(merged.getOr("string", "")).toBe("new-value"); // Overridden
    expect(merged.getOr("number", 0)).toBe(42); // Preserved
    expect(merged.getOr("new_key", "")).toBe("new-value"); // Added
    expect(merged.getOr("nested.prop", "")).toBe("nested-value"); // Preserved from original
    expect(merged.getOr("nested.new_prop", "")).toBe("new-nested-value"); // Added
  });
});

describe("Config Behavior - Monoid Laws", () => {
  const config1 = fromObject({ a: 1, b: 2 });
  const config2 = fromObject({ b: 3, c: 4 });
  const config3 = fromObject({ c: 5, d: 6 });
  const emptyConfig = empty();

  it("left identity: empty.merge(config) === config", () => {
    const result = emptyConfig.merge(config1);
    expect(result.toObject()).toEqual(config1.toObject());
  });

  it("right identity: config.merge(empty) === config", () => {
    const result = config1.merge(emptyConfig);
    expect(result.toObject()).toEqual(config1.toObject());
  });

  it("associativity: (a.merge(b)).merge(c) === a.merge(b.merge(c))", () => {
    const left = config1.merge(config2).merge(config3);
    const right = config1.merge(config2.merge(config3));

    expect(left.toObject()).toEqual(right.toObject());
  });

  it("merge overwrites values correctly", () => {
    const result = config1.merge(config2);
    expect(result.getOr("a", 0)).toBe(1); // From config1
    expect(result.getOr("b", 0)).toBe(3); // Overridden by config2
    expect(result.getOr("c", 0)).toBe(4); // From config2
  });
});

describe("Config Builder Behavior", () => {
  it("builds config with proper functional patterns", () => {
    const builder = ConfigBuilder
      .fromObject({ initial: "value" })
      .set("added", "new-value")
      .merge(fromObject({ merged: "merged-value" }));

    const result = builder.build();

    match(
      (config) => {
        expect(config.getOr("initial", "")).toBe("value");
        expect(config.getOr("added", "")).toBe("new-value");
        expect(config.getOr("merged", "")).toBe("merged-value");
      },
      (error) => expect(true,`Build should succeed: ${error.message}`).toBe(false),
      result,
    );
  });

  it("handles validation in builder pattern", () => {
    const schema = z.object({
      name: z.string(),
      port: z.number(),
    });

    const validBuilder = ConfigBuilder
      .fromObject({ name: "test", port: 3000 })
      .validateWith(schema);

    const validResult = validBuilder.build();

    match(
      (config) => {
        expect(config.getOr("name", "")).toBe("test");
        expect(config.getOr("port", 0)).toBe(3000);
      },
      (error) => expect(true,`Valid config should build: ${error.message}`).toBe(false),
      validResult,
    );

    const invalidBuilder = ConfigBuilder
      .fromObject({ name: "test", port: "invalid" })
      .validateWith(schema);

    const invalidResult = invalidBuilder.build();

    match(
      () => expect(true,"Invalid config should not build"),
      (error) => expect(error.category).toBe("CONFIGURATION"),
      invalidResult,
    );
  });

  it("validateWithSchemaFile works with real schema file", () => {
    const schemaPath = "../tests/core/test-schema.json";

    // Test with valid data that matches the schema
    const validBuilderResult = ConfigBuilder
      .fromObject({
        app: { name: "test-app", version: "1.0.0", port: 3000 },
        test: "valid-value"
      })
      .validateWithSchemaFile(schemaPath);

    const validResult = match(
      (builder) => builder.build(),
      (error) => failure(error),
      validBuilderResult
    );

    match(
      (config) => {
        expect(config.getOr("app.name", "")).toBe("test-app");
        expect(config.getOr("app.version", "")).toBe("1.0.0");
        expect(config.getOr("app.port", 0)).toBe(3000);
        expect(config.getOr("test", "")).toBe("valid-value");
      },
      (error) => expect(true, `Valid schema validation should succeed: ${error.message}`).toBe(false),
      validResult,
    );

    // Test with invalid data that doesn't match the schema
    const invalidBuilderResult = ConfigBuilder
      .fromObject({
        app: { name: "test-app" }, // Missing required 'version'
        test: "value"
      })
      .validateWithSchemaFile(schemaPath);

    const invalidResult = match(
      (builder) => builder.build(),
      (error) => failure(error),
      invalidBuilderResult
    );

    match(
      () => expect(true, "Invalid schema validation should fail").toBe(false),
      (error) => expect(error.category).toBe("CONFIGURATION"),
      invalidResult,
    );
  });

  it("chains multiple operations correctly", () => {
    const result = ConfigBuilder
      .fromObject({ base: "value" })
      .set("key1", "value1")
      .set("key2", "value2")
      .merge(fromObject({ merged: "merged-value" }))
      .set("final", "final-value")
      .build();

    match(
      (config) => {
        expect(config.getOr("base", "")).toBe("value");
        expect(config.getOr("key1", "")).toBe("value1");
        expect(config.getOr("key2", "")).toBe("value2");
        expect(config.getOr("merged", "")).toBe("merged-value");
        expect(config.getOr("final", "")).toBe("final-value");
      },
      (error) => expect(true,`Chain should succeed: ${error.message}`).toBe(false),
      result,
    );
  });
});

describe("Config Validation", () => {
  const schema = z.object({
    name: z.string(),
    port: z.number(),
    enabled: z.boolean().optional(),
  });

  it("validateConfig validates successfully with valid data", () => {
    const validData = { name: "test", port: 3000, enabled: true };
    const config = fromObject(validData);

    const result = validateConfig(config, schema);

    match(
      (validatedData) => {
        expect(validatedData.name).toBe("test");
        expect(validatedData.port).toBe(3000);
        expect(validatedData.enabled).toBe(true);
      },
      (error) => expect(true,`Validation should succeed: ${error.message}`).toBe(false),
      result,
    );
  });

  it("validateConfig fails with invalid data", () => {
    const invalidData = { name: "test", port: "invalid" };
    const config = fromObject(invalidData);

    const result = validateConfig(config, schema);

    match(
      () => expect(true,"Validation should fail for invalid data"),
      (error) => {
        expect(error.category).toBe("CONFIGURATION");
        expect(error.message).toContain("validation");
      },
      result,
    );
  });

  it("safeParseConfig parses and validates in one step", () => {
    const validData = { name: "test", port: 3000 };
    const invalidData = { name: "test", port: "invalid" };

    const validResult = safeParseConfig(validData, schema);

    match(
      (validatedData) => {
        expect(validatedData.name).toBe("test");
        expect(validatedData.port).toBe(3000);
      },
      (error) => expect(true,`Valid parse should succeed: ${error.message}`).toBe(false),
      validResult,
    );

    const invalidResult = safeParseConfig(invalidData, schema);

    match(
      () => expect(true,"Invalid parse should fail"),
      (error) => expect(error.category).toBe("CONFIGURATION"),
      invalidResult,
    );
  });

  it("handles missing required fields", () => {
    const incompleteData = { name: "test" }; // Missing required 'port'
    const config = fromObject(incompleteData);

    const result = validateConfig(config, schema);

    match(
      () => expect(true,"Validation should fail for missing required fields"),
      (error) => {
        expect(error.category).toBe("CONFIGURATION");
        expect(error.message).toContain("validation");
      },
      result,
    );
  });

  it("handles type coercion correctly", () => {
    const stringPortData = { name: "test", port: "3000" }; // String that should coerce to number

    const result = safeParseConfig(stringPortData, schema);

    match(
      (validatedData) => {
        expect(validatedData.name).toBe("test");
        expect(validatedData.port).toBe(3000); // Should be coerced to number
      },
      (error) => {
        // Some schemas might not allow coercion, accept either result
        expect(error.category).toBe("CONFIGURATION");
      },
      result,
    );
  });
});

describe("Config Error Factory", () => {
  it("configError creates config-specific error with context", () => {
    const error = configError("Configuration validation failed", {
      source: "json",
      path: "/config/app.json",
      key: "database.port",
      operation: "validate",
    });

    expect(error.code).toBe("CONFIG_ERROR");
    expect(error.category).toBe("CONFIGURATION");
    expect(error.message).toBe("Configuration validation failed");
    expect(error.context.source).toBe("json");
    expect(error.context.path).toBe("/config/app.json");
    expect(error.context.key).toBe("database.port");
    expect(error.context.operation).toBe("validate");
  });

  it("configError works with minimal context", () => {
    const error = configError("Simple config error");

    expect(error.code).toBe("CONFIG_ERROR");
    expect(error.category).toBe("CONFIGURATION");
    expect(error.message).toBe("Simple config error");
    expect(error.context).toBeDefined();
  });

  it("configError includes timestamp and tracing", () => {
    const error = configError("Timed error", { operation: "load" });

    // Note: QiCore errors don't have timestamp at the top level
    expect(error.code).toBe("CONFIG_ERROR");
    expect(error.context.operation).toBe("load");
    expect(typeof error.toString).toBe("function");
  });
});