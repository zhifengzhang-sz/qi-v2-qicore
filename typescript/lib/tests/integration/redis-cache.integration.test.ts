/**
 * Redis Cache Integration Tests
 * Tests actual Redis connectivity and functionality
 */

import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { createRedisCache, type ICache } from "@qi/core";
import { match, isSuccess, isFailure } from "@qi/base";

// Check if Redis is available
async function isRedisAvailable(): Promise<boolean> {
  const testCache = createRedisCache({
    redis: {
      host: "localhost",
      port: 6379,
      lazyConnect: true,
      connectTimeout: 1000,
      commandTimeout: 1000,
      maxRetriesPerRequest: 1,
    },
  });

  try {
    const result = await testCache.set("redis-availability-test", "test");
    await testCache.delete("redis-availability-test");
    await testCache.close();
    return isSuccess(result);
  } catch {
    await testCache.close();
    return false;
  }
}

describe.skipIf(!(await isRedisAvailable()))("Redis Cache Integration", () => {
  let cache: ICache;
  const testKey = "integration-test-key";
  const testValue = "integration-test-value";

  beforeAll(async () => {
    // Create Redis cache with connection to running Redis instance
    cache = createRedisCache({
      redis: {
        host: "localhost",
        port: 6379,
        lazyConnect: true,
        connectTimeout: 2000,
        commandTimeout: 2000,
        maxRetriesPerRequest: 2,
      },
    });

    // Wait a moment for connection
    await new Promise((resolve) => setTimeout(resolve, 100));
  });

  afterAll(async () => {
    // Clean up test data
    if (cache) {
      await cache.delete(testKey);
      await cache.clear();
      await cache.close(); // Now returns Promise<void>
    }
  });

  it("connects to Redis successfully", async () => {
    // This will fail if Redis is not running or accessible
    const result = await cache.set(testKey, testValue);
    expect(isSuccess(result)).toBe(true);
  });

  it("sets and gets values from Redis", async () => {
    const setResult = await cache.set(`${testKey}-get`, testValue);
    expect(isSuccess(setResult)).toBe(true);

    const getResult = await cache.get(`${testKey}-get`);
    match(
      (value) => expect(value).toBe(testValue),
      (error) => expect(true, `Get should succeed: ${error.message}`).toBe(false),
      getResult,
    );
  });

  it("handles TTL expiration with Redis", async () => {
    const shortTtlKey = `${testKey}-ttl`;

    // Set with 1 second TTL (Redis expects integer seconds)
    const setResult = await cache.set(shortTtlKey, testValue, 1);
    expect(isSuccess(setResult)).toBe(true);

    // Should exist immediately
    const immediateResult = await cache.get(shortTtlKey);
    expect(isSuccess(immediateResult)).toBe(true);

    // Wait for expiration (1.2 seconds to allow for Redis processing time)
    await new Promise((resolve) => setTimeout(resolve, 1200));

    // Should be expired
    const expiredResult = await cache.get(shortTtlKey);
    expect(isFailure(expiredResult)).toBe(true);
  });

  it("deletes keys from Redis", async () => {
    const deleteKey = `${testKey}-delete`;

    await cache.set(deleteKey, testValue);
    const deleteResult = await cache.delete(deleteKey);
    expect(isSuccess(deleteResult)).toBe(true);

    const getResult = await cache.get(deleteKey);
    expect(isFailure(getResult)).toBe(true);
  });

  it("checks key existence in Redis", async () => {
    const existsKey = `${testKey}-exists`;

    // Should not exist initially
    const notExistsResult = await cache.has(existsKey);
    match(
      (exists) => expect(exists).toBe(false),
      (error) => expect(true, `Has check should succeed: ${error.message}`).toBe(false),
      notExistsResult,
    );

    // Set the key
    await cache.set(existsKey, testValue);

    // Should exist now
    const existsResult = await cache.has(existsKey);
    match(
      (exists) => expect(exists).toBe(true),
      (error) => expect(true, `Has check should succeed: ${error.message}`).toBe(false),
      existsResult,
    );
  });

  it("lists keys with pattern matching in Redis", async () => {
    const patternKey1 = "integration:test:1";
    const patternKey2 = "integration:test:2";
    const otherKey = "other:key";

    await cache.set(patternKey1, testValue);
    await cache.set(patternKey2, testValue);
    await cache.set(otherKey, testValue);

    const keysResult = await cache.keys("integration:*");
    match(
      (keys) => {
        expect(keys).toContain(patternKey1);
        expect(keys).toContain(patternKey2);
        expect(keys).not.toContain(otherKey);
      },
      (error) => expect(true, `Keys listing should succeed: ${error.message}`).toBe(false),
      keysResult,
    );

    // Clean up
    await cache.delete(patternKey1);
    await cache.delete(patternKey2);
    await cache.delete(otherKey);
  });

  it("handles Redis connection errors gracefully", async () => {
    // Create cache with invalid port to test error handling
    const badCache = createRedisCache({
      redis: {
        host: "localhost",
        port: 9999, // Invalid port
        connectTimeout: 500,
        commandTimeout: 500,
        lazyConnect: true,
        maxRetriesPerRequest: 1,
      },
    });

    // Should fail gracefully
    const result = await badCache.set("test", "value");
    expect(isFailure(result)).toBe(true);

    await badCache.close();
  }, 5000);

  it("retrieves cache statistics", () => {
    const stats = cache.getStats();

    expect(typeof stats.hits).toBe("number");
    expect(typeof stats.misses).toBe("number");
    expect(typeof stats.sets).toBe("number");
    expect(typeof stats.deletes).toBe("number");
    expect(typeof stats.evictions).toBe("number");
    expect(typeof stats.size).toBe("number");

    // Redis cache should have some operations recorded
    expect(stats.sets).toBeGreaterThan(0);
  });

  it("handles bulk operations with Redis", async () => {
    const bulkKeys = {
      "bulk:1": "value1",
      "bulk:2": "value2",
      "bulk:3": "value3",
    };

    // Test mset (bulk set)
    const msetResult = await cache.mset(bulkKeys);
    expect(isSuccess(msetResult)).toBe(true);

    // Test mget (bulk get)
    const mgetResult = await cache.mget<string>(Object.keys(bulkKeys));
    match(
      (values) => {
        expect(values["bulk:1"]).toBe("value1");
        expect(values["bulk:2"]).toBe("value2");
        expect(values["bulk:3"]).toBe("value3");
      },
      (error) => expect(true, `Bulk get should succeed: ${error.message}`).toBe(false),
      mgetResult,
    );

    // Test mdelete (bulk delete)
    const mdeleteResult = await cache.mdelete(Object.keys(bulkKeys));
    match(
      (deletedCount) => expect(deletedCount).toBe(3),
      (error) => expect(true, `Bulk delete should succeed: ${error.message}`).toBe(false),
      mdeleteResult,
    );
  });

  it("tests Redis persistence across operations", async () => {
    const persistKey = "persist-test";
    const persistValue = { complex: "object", number: 42, nested: { array: [1, 2, 3] } };

    // Set complex object
    const setResult = await cache.set(persistKey, persistValue);
    expect(isSuccess(setResult)).toBe(true);

    // Get it back and verify structure
    const getResult = await cache.get<typeof persistValue>(persistKey);
    match(
      (value) => {
        expect(value.complex).toBe("object");
        expect(value.number).toBe(42);
        expect(value.nested.array).toEqual([1, 2, 3]);
      },
      (error) => expect(true, `Complex object persistence should work: ${error.message}`).toBe(false),
      getResult,
    );

    // Clean up
    await cache.delete(persistKey);
  });

  it("verifies Redis connection and error handling", async () => {
    // Test successful connection with valid Redis server
    const connectionCache = createRedisCache({
      redis: {
        host: "localhost",
        port: 6379,
        lazyConnect: true,
        connectTimeout: 2000,
      },
    });

    // Verify connection works by performing operation
    const result = await connectionCache.set("connection-test", "value");
    expect(isSuccess(result)).toBe(true);

    // Verify we can read back the value
    const getResult = await connectionCache.get("connection-test");
    match(
      (value) => expect(value).toBe("value"),
      (error) => expect(true, `Should retrieve connected value: ${error.message}`).toBe(false),
      getResult,
    );

    // Clean up
    await connectionCache.delete("connection-test");
    await connectionCache.close();
  });
});
