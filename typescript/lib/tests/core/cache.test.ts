/**
 * Cache Tests - Contract Compliance with Proper Functional Patterns
 * Focus: Law, Interfaces, Behavior using Result<T> patterns
 */

import { describe, it, expect, vi, beforeEach } from "vitest";
import { SAMPLE_DATA } from "../test-constants.js";
import {
  MemoryCache,
  RedisCache,
  createCache,
  createMemoryCache,
  createRedisCache,
  createPersistent,
  cacheAside,
  cacheError,
  type CacheConfig,
} from "@qi/core";
import { match, isSuccess, isFailure, success, failure } from "@qi/base";

describe("Cache Factory Operations", () => {
  it("createCache with memory backend provides working cache", async () => {
    const config: CacheConfig = {
      backend: "memory",
      maxSize: 100,
      defaultTtl: 60,
    };

    const cacheResult = createCache(config);
    expect(isSuccess(cacheResult)).toBe(true);

    // Use proper functional pattern instead of direct access
    await match(
      async (cache) => {
        // Test behavior, not implementation details
        await cache.set(SAMPLE_DATA.CACHE_KEYS.BASIC, SAMPLE_DATA.CACHE_VALUES.BASIC);
        const result = await cache.get(SAMPLE_DATA.CACHE_KEYS.BASIC);

        match(
          (value) => expect(value).toBe(SAMPLE_DATA.CACHE_VALUES.BASIC),
          (error) => expect.fail(`Should not fail: ${error.message}`),
          result,
        );
      },
      (error) => expect.fail(`Cache creation should succeed: ${error.message}`),
      cacheResult,
    );
  });

  it("createCache creates redis cache", () => {
    const config: CacheConfig = {
      backend: "redis",
      defaultTtl: 60,
      redis: { host: "localhost", port: 6379 },
    };

    const cacheResult = createCache(config);

    match(
      (cache) => expect(cache).toBeInstanceOf(RedisCache),
      (error) => expect.fail(`Cache creation should succeed: ${error.message}`),
      cacheResult,
    );
  });

  it("createMemoryCache creates memory cache", () => {
    const cache = createMemoryCache({ maxSize: 100 });
    expect(cache).toBeInstanceOf(MemoryCache);
  });

  it("createRedisCache creates redis cache", () => {
    const cache = createRedisCache({ redis: { host: "localhost" } });
    expect(cache).toBeInstanceOf(RedisCache);
  });

  it("createPersistent creates persistent redis cache", () => {
    const result = createPersistent("/tmp/test.cache", { maxSize: 100 });

    match(
      (cache) => expect(cache).toBeInstanceOf(RedisCache),
      (error) => expect.fail(`Persistent cache creation should succeed: ${error.message}`),
      result,
    );
  });

  it("createPersistent handles errors gracefully", () => {
    // Test with empty path (should still succeed)
    const result = createPersistent("", { maxSize: 100 });
    expect(isSuccess(result)).toBe(true);
  });
});

describe("Cache Interface Operations", () => {
  const cache = createMemoryCache({ maxSize: 100, defaultTtl: 60 });

  it("cache has all required methods", () => {
    expect(typeof cache.get).toBe("function");
    expect(typeof cache.set).toBe("function");
    expect(typeof cache.delete).toBe("function");
    expect(typeof cache.has).toBe("function");
    expect(typeof cache.remove).toBe("function");
    expect(typeof cache.size).toBe("function");
    expect(typeof cache.clear).toBe("function");
    expect(typeof cache.keys).toBe("function");
    expect(typeof cache.getStats).toBe("function");
    expect(typeof cache.close).toBe("function");
  });
});

describe("Cache Behavior Contract", () => {
  let cache: MemoryCache;

  beforeEach(() => {
    cache = createMemoryCache({ maxSize: 10, defaultTtl: 60 });
  });

  it("get returns failure for non-existent key", async () => {
    const result = await cache.get("non-existent");
    expect(isFailure(result)).toBe(true);
  });

  it("set and get work correctly", async () => {
    const setResult = await cache.set("key1", "value1");
    expect(isSuccess(setResult)).toBe(true);

    const getResult = await cache.get("key1");

    match(
      (value) => expect(value).toBe("value1"),
      (error) => expect.fail(`Get should succeed: ${error.message}`),
      getResult,
    );
  });

  it("delete removes existing key", async () => {
    await cache.set("key1", "value1");
    const deleteResult = await cache.delete("key1");

    match(
      (wasDeleted) => expect(wasDeleted).toBe(true),
      (error) => expect.fail(`Delete should succeed: ${error.message}`),
      deleteResult,
    );

    const getResult = await cache.get("key1");
    expect(isFailure(getResult)).toBe(true);
  });

  it("delete returns false for non-existent key", async () => {
    const deleteResult = await cache.delete("non-existent");

    match(
      (wasDeleted) => expect(wasDeleted).toBe(false),
      (error) => expect.fail(`Delete should succeed: ${error.message}`),
      deleteResult,
    );
  });

  it("has returns true for existing key", async () => {
    await cache.set("key1", "value1");
    const hasResult = await cache.has("key1");

    match(
      (exists) => expect(exists).toBe(true),
      (error) => expect.fail(`Has should succeed: ${error.message}`),
      hasResult,
    );
  });

  it("has returns false for non-existent key", async () => {
    const hasResult = await cache.has("non-existent");

    match(
      (exists) => expect(exists).toBe(false),
      (error) => expect.fail(`Has should succeed: ${error.message}`),
      hasResult,
    );
  });

  it("size returns correct count", async () => {
    await cache.set("key1", "value1");
    await cache.set("key2", "value2");

    const sizeResult = await cache.size();

    match(
      (size) => expect(size).toBe(2),
      (error) => expect.fail(`Size should succeed: ${error.message}`),
      sizeResult,
    );
  });

  it("clear removes all entries", async () => {
    await cache.set("key1", "value1");
    await cache.set("key2", "value2");

    const clearResult = await cache.clear();
    expect(isSuccess(clearResult)).toBe(true);

    const sizeResult = await cache.size();

    match(
      (size) => expect(size).toBe(0),
      (error) => expect.fail(`Size should succeed: ${error.message}`),
      sizeResult,
    );
  });

  it("keys returns all keys", async () => {
    await cache.set("key1", "value1");
    await cache.set("key2", "value2");

    const keysResult = await cache.keys();

    match(
      (keys) => {
        expect(keys).toHaveLength(2);
        expect(keys).toContain("key1");
        expect(keys).toContain("key2");
      },
      (error) => expect.fail(`Keys should succeed: ${error.message}`),
      keysResult,
    );
  });

  it("TTL expiration works", async () => {
    const shortTtl = 0.1; // 100ms
    await cache.set("expiring-key", "value", shortTtl);

    // Should exist immediately
    const immediateResult = await cache.get("expiring-key");
    expect(isSuccess(immediateResult)).toBe(true);

    // Wait for expiration
    await new Promise(resolve => setTimeout(resolve, 150));

    // Should be expired
    const expiredResult = await cache.get("expiring-key");
    expect(isFailure(expiredResult)).toBe(true);
  });
});

describe("Cache LRU Behavior", () => {
  it("evicts least recently used items when cache is full", async () => {
    const cache = createMemoryCache({ maxSize: 2 });

    await cache.set("key1", "value1");
    await cache.set("key2", "value2");

    // Access key1 to make it more recently used
    await cache.get("key1");

    // Add key3, should evict key2 (least recently used)
    await cache.set("key3", "value3");

    // key1 and key3 should exist, key2 should be evicted
    const key1Result = await cache.get("key1");
    expect(isSuccess(key1Result)).toBe(true);

    const key2Result = await cache.get("key2");
    expect(isFailure(key2Result)).toBe(true);

    const key3Result = await cache.get("key3");
    expect(isSuccess(key3Result)).toBe(true);
  });
});

describe("Cache Utility Functions", () => {
  it("cacheAside pattern works with cache hit", async () => {
    const cache = createMemoryCache({ maxSize: 100 });
    await cache.set("cached-key", "cached-value");

    const loader = vi.fn().mockResolvedValue("loaded-value");

    const result = await cacheAside("cached-key", cache, loader);

    match(
      (value) => {
        expect(value).toBe("cached-value");
        expect(loader).not.toHaveBeenCalled();
      },
      (error) => expect.fail(`Cache aside should succeed: ${error.message}`),
      result,
    );
  });

  it("cacheAside pattern works with cache miss", async () => {
    const cache = createMemoryCache({ maxSize: 100 });
    const loader = vi.fn().mockResolvedValue("loaded-value");

    const result = await cacheAside("missing-key", cache, loader);

    match(
      (value) => {
        expect(value).toBe("loaded-value");
        expect(loader).toHaveBeenCalledOnce();
      },
      (error) => expect.fail(`Cache aside should succeed: ${error.message}`),
      result,
    );

    // Verify it was cached
    const cachedResult = await cache.get("missing-key");

    match(
      (value) => expect(value).toBe("loaded-value"),
      (error) => expect.fail(`Value should be cached: ${error.message}`),
      cachedResult,
    );
  });
});

describe("Cache Error Factory", () => {
  it("cacheError creates proper error", () => {
    const error = cacheError("Test error", {
      operation: "get",
      key: "test-key",
      backend: "memory",
    });

    expect(error.code).toBe("CACHE_ERROR");
    expect(error.message).toBe("Test error");
    expect(error.category).toBe("RESOURCE");
    expect(error.context.operation).toBe("get");
    expect(error.context.key).toBe("test-key");
    expect(error.context.backend).toBe("memory");
  });
});

describe("Cache Batch Operations", () => {
  let cache: MemoryCache;

  beforeEach(() => {
    cache = createMemoryCache({ maxSize: 100 });
  });

  describe("mget", () => {
    it("returns values for existing keys", async () => {
      await cache.set("key1", "value1");
      await cache.set("key2", "value2");
      await cache.set("key3", "value3");

      const result = await cache.mget(["key1", "key2", "key3"]);

      match(
        (values) => {
          expect(values.key1).toBe("value1");
          expect(values.key2).toBe("value2");
          expect(values.key3).toBe("value3");
        },
        (error) => expect.fail(`Mget should succeed: ${error.message}`),
        result,
      );
    });

    it("handles mixed existing and non-existing keys", async () => {
      await cache.set("key1", "value1");
      await cache.set("key3", "value3");

      const result = await cache.mget(["key1", "key2", "key3"]);

      match(
        (values) => {
          expect(values.key1).toBe("value1");
          expect(values.key2).toBeUndefined();
          expect(values.key3).toBe("value3");
        },
        (error) => expect.fail(`Mget should succeed: ${error.message}`),
        result,
      );
    });
  });

  describe("mset", () => {
    it("sets multiple key-value pairs", async () => {
      const entries = {
        key1: "value1",
        key2: "value2",
        key3: "value3",
      };

      const setResult = await cache.mset(entries);
      expect(isSuccess(setResult)).toBe(true);

      // Verify all keys were set
      const getResult1 = await cache.get("key1");
      const getResult2 = await cache.get("key2");
      const getResult3 = await cache.get("key3");

      match(
        (value) => expect(value).toBe("value1"),
        () => expect.fail("key1 should exist"),
        getResult1,
      );

      match(
        (value) => expect(value).toBe("value2"),
        () => expect.fail("key2 should exist"),
        getResult2,
      );

      match(
        (value) => expect(value).toBe("value3"),
        () => expect.fail("key3 should exist"),
        getResult3,
      );
    });

    it("sets multiple key-value pairs with TTL", async () => {
      const entries = {
        key1: "value1",
        key2: "value2",
      };

      const setResult = await cache.mset(entries, 0.1); // 100ms TTL
      expect(isSuccess(setResult)).toBe(true);

      // Should exist immediately
      const immediateResult = await cache.get("key1");
      expect(isSuccess(immediateResult)).toBe(true);

      // Wait for expiration
      await new Promise(resolve => setTimeout(resolve, 150));

      // Should be expired
      const expiredResult = await cache.get("key1");
      expect(isFailure(expiredResult)).toBe(true);
    });
  });

  describe("mdelete", () => {
    it("deletes existing keys and returns count", async () => {
      await cache.set("key1", "value1");
      await cache.set("key2", "value2");
      await cache.set("key3", "value3");

      const deleteResult = await cache.mdelete(["key1", "key2", "key3"]);

      match(
        (deletedCount) => expect(deletedCount).toBe(3),
        (error) => expect.fail(`Mdelete should succeed: ${error.message}`),
        deleteResult,
      );

      // Verify keys are deleted
      const getResult1 = await cache.get("key1");
      const getResult2 = await cache.get("key2");
      const getResult3 = await cache.get("key3");

      expect(isFailure(getResult1)).toBe(true);
      expect(isFailure(getResult2)).toBe(true);
      expect(isFailure(getResult3)).toBe(true);
    });

    it("handles mixed existing and non-existing keys", async () => {
      await cache.set("key1", "value1");
      await cache.set("key3", "value3");

      const deleteResult = await cache.mdelete(["key1", "key2", "key3"]);

      match(
        (deletedCount) => expect(deletedCount).toBe(2), // Only key1 and key3 existed
        (error) => expect.fail(`Mdelete should succeed: ${error.message}`),
        deleteResult,
      );
    });
  });
});

describe("Cache Advanced Operations", () => {
  let cache: MemoryCache;

  beforeEach(() => {
    cache = createMemoryCache({ maxSize: 100 });
  });

  describe("getOrSet", () => {
    it("returns cached value if exists", async () => {
      await cache.set("existing-key", "cached-value");

      const factory = vi.fn().mockResolvedValue(success("factory-value"));

      const result = await cache.getOrSet("existing-key", factory);

      match(
        (value) => {
          expect(value).toBe("cached-value");
          expect(factory).not.toHaveBeenCalled();
        },
        (error) => expect.fail(`GetOrSet should succeed: ${error.message}`),
        result,
      );
    });

    it("calls factory and caches result if key doesn't exist", async () => {
      const factory = vi.fn().mockResolvedValue(success("factory-value"));

      const result = await cache.getOrSet("missing-key", factory);

      match(
        (value) => {
          expect(value).toBe("factory-value");
          expect(factory).toHaveBeenCalledOnce();
        },
        (error) => expect.fail(`GetOrSet should succeed: ${error.message}`),
        result,
      );

      // Verify it was cached
      const cachedResult = await cache.get("missing-key");

      match(
        (value) => expect(value).toBe("factory-value"),
        (error) => expect.fail(`Value should be cached: ${error.message}`),
        cachedResult,
      );
    });

    it("handles factory failure gracefully", async () => {
      const factoryError = cacheError("Factory failed", { operation: "factory" });
      const factory = vi.fn().mockResolvedValue(failure(factoryError));

      const result = await cache.getOrSet("missing-key", factory);

      match(
        () => expect.fail("Should not succeed when factory fails"),
        (error) => {
          expect(error.message).toBe("Factory failed");
          expect(factory).toHaveBeenCalledOnce();
        },
        result,
      );

      // Verify nothing was cached
      const cachedResult = await cache.get("missing-key");
      expect(isFailure(cachedResult)).toBe(true);
    });
  });
});