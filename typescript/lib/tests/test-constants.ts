/**
 * Centralized Test Constants
 *
 * Common test data, delays, and configuration values used across test suites.
 * Prevents magic numbers and promotes consistency.
 */

// ============================================================================
// Timing Constants
// ============================================================================

export const TEST_DELAYS = {
  /** Very short delay for quick async operations (1ms) */
  SHORT: 1,

  /** Medium delay for network simulation (10ms) */
  MEDIUM: 10,

  /** Timing buffer for race condition tests (9ms) */
  TIMING_BUFFER: 9,

  /** Redis integration test TTL (1 second) */
  REDIS_TTL: 1,

  /** Test timeout buffer (1.2 seconds in ms) */
  TTL_BUFFER: 1200,
} as const

// ============================================================================
// Test Data Factories
// ============================================================================

export const TEST_ERRORS = {
  /** Standard test error */
  STANDARD: {
    code: 'TEST_ERROR',
    message: 'Test error message',
    category: 'SYSTEM' as const,
  },

  /** Async operation test error */
  ASYNC: {
    code: 'ASYNC_ERROR',
    message: 'Async operation failed',
    category: 'SYSTEM' as const,
  },

  /** Validation test error */
  VALIDATION: {
    code: 'VALIDATION_ERROR',
    message: 'Validation failed',
    category: 'VALIDATION' as const,
  },
} as const

// ============================================================================
// Configuration Constants
// ============================================================================

export const TEST_CONFIG = {
  /** Property-based test run configurations */
  PROPERTY_RUNS: {
    /** Standard property test runs */
    STANDARD: 100,

    /** Intensive property test runs */
    INTENSIVE: 500,

    /** Quick property test runs for timing-sensitive tests */
    QUICK: 50,

    /** Full mathematical law verification runs */
    MATHEMATICAL: 1000,
  },

  /** Cache test configurations */
  CACHE: {
    /** Small cache size for LRU testing */
    SMALL_SIZE: 2,

    /** Medium cache size for general testing */
    MEDIUM_SIZE: 10,

    /** Default TTL for cache tests (1 second) */
    DEFAULT_TTL: 1,
  },

  /** Logger test configurations */
  LOGGER: {
    /** Test logger name */
    NAME: 'test-logger',

    /** Performance test iteration count */
    PERFORMANCE_ITERATIONS: 1000,
  },
} as const

// ============================================================================
// Sample Test Data
// ============================================================================

export const SAMPLE_DATA = {
  /** Sample user data for testing */
  USER: {
    id: '123',
    name: 'Test User',
    email: 'test@example.com',
  },

  /** Sample cache keys */
  CACHE_KEYS: {
    BASIC: 'test-key',
    ERROR: 'error-key',
  },

  /** Sample cache values */
  CACHE_VALUES: {
    BASIC: 'test-value',
    LOADED: 'loaded-value',
  },
} as const

// ============================================================================
// Time Helper Functions
// ============================================================================

/**
 * Standard delay helper for consistent async testing
 */
export const delay = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms))
