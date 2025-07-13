# QiCore Base Component - TypeScript Implementation Specification

This document provides TypeScript-specific implementation guidance for the QiCore Base Component, mapping the language-agnostic contracts from [qi.base.contracts.md](../../docs/contracts/qi.base.contracts.md) to idiomatic TypeScript patterns.

## Language Mapping Overview

| Contract Operation | TypeScript Implementation | TypeScript 5.8 Features |
|-------------------|---------------------------|-------------------------|
| `Result<T>` | `Result<T, QiError>` | Conditional return types |
| `andThen` | `.andThen(fn)` | Better async inference |
| `asyncMap` | `Promise<Result<T>>` | Node.js interop |
| `inspect` | `.tap(fn)` | Enhanced debugging |
| `ErrorCategory` | `enum ErrorCategory` | String literal unions |

## TypeScript-Specific Result Implementation

### Core Types

```typescript
// Result<T> implementation using discriminated unions
export type Result<T, E = QiError> = Success<T> | Failure<E>;

export interface Success<T> {
  readonly kind: 'success';
  readonly value: T;
}

export interface Failure<E> {
  readonly kind: 'failure';  
  readonly error: E;
}

// Type guards for TypeScript
export const isSuccess = <T, E>(result: Result<T, E>): result is Success<T> =>
  result.kind === 'success';

export const isFailure = <T, E>(result: Result<T, E>): result is Failure<E> =>
  result.kind === 'failure';
```

### Factory Functions

```typescript
// Factory functions with TypeScript inference
export const success = <T>(value: T): Result<T, never> => ({
  kind: 'success',
  value,
});

export const failure = <E>(error: E): Result<never, E> => ({
  kind: 'failure',
  error,
});

// TypeScript 5.8 conditional return type support
export function fromTryCatch<T>(fn: () => T): Result<T, QiError> {
  try {
    const value = fn();
    return success(value);
  } catch (error) {
    return failure(QiError.fromError(error));
  }
}

// Modern async handling with TypeScript 5.8 Node.js interop
export async function fromPromise<T>(promise: Promise<T>): Promise<Result<T, QiError>> {
  try {
    const value = await promise;
    return success(value);
  } catch (error) {
    return failure(QiError.fromError(error));
  }
}
```

### Result Methods

```typescript
// Functor implementation
export class ResultImpl<T, E> {
  constructor(private readonly result: Result<T, E>) {}

  map<U>(fn: (value: T) => U): Result<U, E> {
    return this.result.kind === 'success'
      ? success(fn(this.result.value))
      : this.result;
  }

  // Rust-style andThen (more intuitive than flatMap)
  andThen<U>(fn: (value: T) => Result<U, E>): Result<U, E> {
    return this.result.kind === 'success'
      ? fn(this.result.value)
      : this.result;
  }

  // Side effect operations (TypeScript tap pattern)
  inspect(fn: (value: T) => void): Result<T, E> {
    if (this.result.kind === 'success') {
      fn(this.result.value);
    }
    return this.result;
  }

  inspectErr(fn: (error: E) => void): Result<T, E> {
    if (this.result.kind === 'failure') {
      fn(this.result.error);
    }
    return this.result;
  }

  // Pattern matching with TypeScript discrimination
  match<U>(onSuccess: (value: T) => U, onFailure: (error: E) => U): U {
    return this.result.kind === 'success'
      ? onSuccess(this.result.value)
      : onFailure(this.result.error);
  }

  // Extraction with TypeScript error handling
  unwrap(): T {
    if (this.result.kind === 'failure') {
      throw new Error(`Attempted to unwrap a failure: ${this.result.error}`);
    }
    return this.result.value;
  }

  unwrapOr(defaultValue: T): T {
    return this.result.kind === 'success' ? this.result.value : defaultValue;
  }
}
```

### TypeScript 5.8 Async Patterns

```typescript
// Async Result operations with modern TypeScript
export class AsyncResult<T, E> {
  constructor(private readonly promise: Promise<Result<T, E>>) {}

  async map<U>(fn: (value: T) => U | Promise<U>): Promise<Result<U, E>> {
    const result = await this.promise;
    if (result.kind === 'failure') {
      return result;
    }
    try {
      const mapped = await fn(result.value);
      return success(mapped);
    } catch (error) {
      return failure(QiError.fromError(error) as E);
    }
  }

  async andThen<U>(fn: (value: T) => Promise<Result<U, E>>): Promise<Result<U, E>> {
    const result = await this.promise;
    return result.kind === 'success' ? fn(result.value) : result;
  }

  // Combining multiple async Results (TypeScript 5.8 inference)
  static async all<T extends readonly unknown[], E>(
    results: { [K in keyof T]: Promise<Result<T[K], E>> }
  ): Promise<Result<T, E>> {
    const resolved = await Promise.all(results);
    
    for (const result of resolved) {
      if (result.kind === 'failure') {
        return result;
      }
    }
    
    return success(resolved.map(r => (r as Success<any>).value) as T);
  }
}
```

### Collection Operations

```typescript
// TypeScript utility types for Result collections
export namespace ResultArrayUtils {
  // Partition with TypeScript tuple return type
  export function partition<T, E>(
    results: Result<T, E>[]
  ): [T[], E[]] {
    const successes: T[] = [];
    const failures: E[] = [];
    
    for (const result of results) {
      if (result.kind === 'success') {
        successes.push(result.value);
      } else {
        failures.push(result.error);
      }
    }
    
    return [successes, failures];
  }

  // Sequence with fail-fast semantics
  export function sequence<T, E>(results: Result<T, E>[]): Result<T[], E> {
    const values: T[] = [];
    
    for (const result of results) {
      if (result.kind === 'failure') {
        return result;
      }
      values.push(result.value);
    }
    
    return success(values);
  }

  // Rights extraction (successes only)
  export function rights<T, E>(results: Result<T, E>[]): T[] {
    return results
      .filter(isSuccess)
      .map(result => result.value);
  }

  // Lefts extraction (failures only)
  export function lefts<T, E>(results: Result<T, E>[]): E[] {
    return results
      .filter(isFailure)
      .map(result => result.error);
  }
}
```

## QiError TypeScript Implementation

### Enhanced Error Types

```typescript
// TypeScript 5.8 string literal union for categories
export type ErrorCategory = 
  | 'VALIDATION'
  | 'NETWORK' 
  | 'SYSTEM'
  | 'BUSINESS'
  | 'SECURITY'
  | 'PARSING'
  | 'TIMEOUT'
  | 'ASYNC'
  | 'CONCURRENCY'
  | 'RESOURCE'
  | 'CONFIGURATION'
  | 'SERIALIZATION'
  | 'FILESYSTEM'
  | 'UNKNOWN';

export interface QiErrorData {
  readonly code: string;
  readonly message: string;
  readonly category: ErrorCategory;
  readonly context: Record<string, unknown>;
  readonly cause?: QiError | Error;
  readonly timestamp: Date;
}

export class QiError extends Error implements QiErrorData {
  readonly code: string;
  readonly category: ErrorCategory;
  readonly context: Record<string, unknown>;
  readonly cause?: QiError | Error;
  readonly timestamp: Date;

  constructor(
    code: string,
    message: string,
    category: ErrorCategory,
    context: Record<string, unknown> = {},
    cause?: QiError | Error,
    timestamp: Date = new Date()
  ) {
    super(message);
    this.name = 'QiError';
    this.code = code;
    this.category = category;
    this.context = context;
    this.cause = cause;
    this.timestamp = timestamp;
  }

  // TypeScript factory methods
  static create(
    code: string,
    message: string,
    category: ErrorCategory,
    context?: Record<string, unknown>
  ): QiError {
    return new QiError(code, message, category, context);
  }

  static fromError(error: unknown, category: ErrorCategory = 'UNKNOWN'): QiError {
    if (error instanceof QiError) {
      return error;
    }
    
    if (error instanceof Error) {
      return new QiError(
        'WRAPPED_ERROR',
        error.message,
        category,
        { originalError: error.name },
        error
      );
    }
    
    return new QiError(
      'UNKNOWN_ERROR',
      String(error),
      category,
      { originalValue: error }
    );
  }

  // Immutable transformation methods
  withContext(additionalContext: Record<string, unknown>): QiError {
    return new QiError(
      this.code,
      this.message,
      this.category,
      { ...this.context, ...additionalContext },
      this.cause,
      this.timestamp
    );
  }

  withCause(cause: QiError | Error): QiError {
    return new QiError(
      this.code,
      this.message,
      this.category,
      this.context,
      cause,
      this.timestamp
    );
  }

  // TypeScript-specific chain operation
  chain(effect: QiError): QiError {
    return effect.withCause(this);
  }

  // Type-safe category checking
  hasCategory(category: ErrorCategory): boolean {
    return this.category === category;
  }

  // Error chain navigation
  getRootError(): QiError {
    let current: QiError = this;
    while (current.cause instanceof QiError) {
      current = current.cause;
    }
    return current;
  }

  // Serialization support
  toJSON(): object {
    return {
      code: this.code,
      message: this.message,
      category: this.category,
      context: this.context,
      timestamp: this.timestamp.toISOString(),
      ...(this.cause && { 
        cause: this.cause instanceof QiError 
          ? this.cause.toJSON() 
          : this.cause.toString() 
      })
    };
  }

  // Human-readable chain formatting
  formatChain(): string {
    const chain: string[] = [];
    let current: QiError | Error | undefined = this;
    
    while (current) {
      if (current instanceof QiError) {
        chain.push(`[${current.category}] ${current.code}: ${current.message}`);
        current = current.cause;
      } else if (current instanceof Error) {
        chain.push(`[WRAPPED] ${current.name}: ${current.message}`);
        break;
      } else {
        break;
      }
    }
    
    return chain.join(' → ');
  }
}
```

## Integration with TypeScript Ecosystem

### Promise Integration

```typescript
// Integration with modern TypeScript Promise patterns
export namespace PromiseResult {
  // Convert Promise<T> to Promise<Result<T>>
  export async function wrap<T>(promise: Promise<T>): Promise<Result<T, QiError>> {
    try {
      const value = await promise;
      return success(value);
    } catch (error) {
      return failure(QiError.fromError(error));
    }
  }

  // Convert Result<T> back to Promise<T> (for compatibility)
  export function unwrap<T>(result: Result<T, QiError>): Promise<T> {
    return result.kind === 'success'
      ? Promise.resolve(result.value)
      : Promise.reject(result.error);
  }
}
```

### Utility Types

```typescript
// TypeScript utility types for Result
export type ResultValue<R> = R extends Result<infer T, any> ? T : never;
export type ResultError<R> = R extends Result<any, infer E> ? E : never;

// Conditional type for async Results
export type AsyncResult<T, E = QiError> = Promise<Result<T, E>>;

// Helper for function return types
export type Fallible<T, E = QiError> = Result<T, E>;
export type AsyncFallible<T, E = QiError> = AsyncResult<T, E>;
```

## Usage Examples

See [examples/](./examples/) directory for complete TypeScript usage patterns including:
- Basic Result operations
- Async/await integration  
- Error handling patterns
- Integration with popular TypeScript libraries
- Testing patterns with Vitest

## Compatibility Notes

- **TypeScript 5.8+**: Full support for conditional return types and enhanced async inference
- **Node.js 18+**: Required for optimal async/await performance
- **ESM/CommonJS**: Dual package support via tsup build configuration
- **Testing**: Vitest with property-based testing via fast-check

This specification ensures TypeScript developers can use QiCore idiomatically while maintaining full compliance with the language-agnostic behavioral contracts.