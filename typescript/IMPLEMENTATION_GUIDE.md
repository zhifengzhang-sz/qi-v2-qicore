# QiCore Foundation TypeScript Implementation Guide

## Getting Started

### Project Setup

```bash
# Create TypeScript workspace
mkdir qi-qicore-typescript
cd qi-qicore-typescript

# Initialize project
npm init -y
npm install typescript @types/node vitest tsup --save-dev
npm install yaml --save

# Copy configuration files
cp ../qi-v2-qicore/typescript/package.json .
cp ../qi-v2-qicore/typescript/tsconfig.json .
```

### Directory Structure

```
src/
├── base/                     # qi/base implementation
│   ├── result.ts            # Result<T> monad
│   ├── error.ts             # QiError type
│   └── index.ts             # Base exports
├── core/                     # qi/core implementation  
│   ├── config.ts            # Configuration service
│   ├── logger.ts            # Logging service
│   ├── cache.ts             # Caching service
│   └── index.ts             # Core exports
└── index.ts                  # Main exports
```

## Implementation Steps

### Step 1: Result<T> Monad (src/base/result.ts)

```typescript
// Base Result type following Haskell implementation
export abstract class Result<T> {
  abstract isSuccess(): boolean;
  abstract isFailure(): boolean;
  abstract map<U>(fn: (value: T) => U): Result<U>;
  abstract flatMap<U>(fn: (value: T) => Result<U>): Result<U>;
  abstract match<U>(onSuccess: (value: T) => U, onFailure: (error: QiError) => U): U;
  abstract unwrap(): T;
  abstract unwrapOr(defaultValue: T): T;
  abstract getError(): QiError | null;
}

export class Success<T> extends Result<T> {
  constructor(private readonly value: T) {
    super();
  }
  
  isSuccess(): boolean { return true; }
  isFailure(): boolean { return false; }
  
  map<U>(fn: (value: T) => U): Result<U> {
    return new Success(fn(this.value));
  }
  
  flatMap<U>(fn: (value: T) => Result<U>): Result<U> {
    return fn(this.value);
  }
  
  match<U>(onSuccess: (value: T) => U, onFailure: (error: QiError) => U): U {
    return onSuccess(this.value);
  }
  
  unwrap(): T {
    return this.value;
  }
  
  unwrapOr(defaultValue: T): T {
    return this.value;
  }
  
  getError(): QiError | null {
    return null;
  }
}

export class Failure<T = never> extends Result<T> {
  constructor(private readonly error: QiError) {
    super();
  }
  
  isSuccess(): boolean { return false; }
  isFailure(): boolean { return true; }
  
  map<U>(fn: (value: T) => U): Result<U> {
    return new Failure<U>(this.error);
  }
  
  flatMap<U>(fn: (value: T) => Result<U>): Result<U> {
    return new Failure<U>(this.error);
  }
  
  match<U>(onSuccess: (value: T) => U, onFailure: (error: QiError) => U): U {
    return onFailure(this.error);
  }
  
  unwrap(): T {
    throw new Error(`Attempted to unwrap a Failure: ${this.error.message}`);
  }
  
  unwrapOr(defaultValue: T): T {
    return defaultValue;
  }
  
  getError(): QiError {
    return this.error;
  }
}

// Factory functions
export const success = <T>(value: T): Result<T> => new Success(value);
export const failure = <T = never>(error: QiError): Result<T> => new Failure<T>(error);
```

### Step 2: QiError Type (src/base/error.ts)

```typescript
export enum ErrorCategory {
  VALIDATION = 'VALIDATION',
  NETWORK = 'NETWORK', 
  SYSTEM = 'SYSTEM',
  BUSINESS = 'BUSINESS',
  SECURITY = 'SECURITY',
  PARSING = 'PARSING',
  TIMEOUT = 'TIMEOUT',
  UNKNOWN = 'UNKNOWN'
}

export class QiError extends Error {
  public readonly code: string;
  public readonly category: ErrorCategory;
  public readonly context: Record<string, unknown>;
  public readonly cause?: QiError | Error;
  public readonly timestamp: Date;
  
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
  
  static create(
    code: string,
    message: string,
    category: ErrorCategory,
    context: Record<string, unknown> = {}
  ): QiError {
    return new QiError(code, message, category, context);
  }
  
  static fromError(error: Error, category: ErrorCategory = ErrorCategory.UNKNOWN): QiError {
    return new QiError(
      'UNKNOWN_ERROR',
      error.message,
      category,
      { originalError: error.name },
      error
    );
  }
  
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
  
  toString(): string {
    return `QiError[${this.code}]: ${this.message}`;
  }
  
  toJSON(): object {
    return {
      code: this.code,
      message: this.message,
      category: this.category,
      context: this.context,
      timestamp: this.timestamp.toISOString(),
      ...(this.cause && { cause: this.cause.toString() })
    };
  }
}
```

### Step 3: Configuration Service (src/core/config.ts)

```typescript
import { Result, success, failure, QiError, ErrorCategory } from '../base';
import { promises as fs } from 'fs';
import yaml from 'yaml';

export type ConfigValue = string | number | boolean | ConfigValue[] | ConfigObject | null;
export interface ConfigObject { [key: string]: ConfigValue; }

export class ConfigData {
  constructor(
    private readonly data: ConfigObject,
    public readonly timestamp: Date,
    public readonly source: string
  ) {}
  
  get<T>(key: string): Result<T> {
    const value = this.getNestedValue(key);
    if (value === undefined) {
      return failure(QiError.create(
        'NOT_FOUND',
        `Key not found: ${key}`,
        ErrorCategory.VALIDATION,
        { key }
      ));
    }
    return success(value as T);
  }
  
  getWithDefault<T>(key: string, defaultValue: T): T {
    return this.get<T>(key).unwrapOr(defaultValue);
  }
  
  has(key: string): boolean {
    return this.getNestedValue(key) !== undefined;
  }
  
  keys(): string[] {
    return this.flattenKeys('', this.data).sort();
  }
  
  static merge(configs: ConfigData[]): ConfigData {
    if (configs.length === 0) {
      return ConfigData.empty();
    }
    
    const mergedData = configs.reduce(
      (acc, config) => this.mergeObjects(acc, config.data),
      {} as ConfigObject
    );
    
    const latestTimestamp = configs.reduce(
      (latest, config) => config.timestamp > latest ? config.timestamp : latest,
      configs[0].timestamp
    );
    
    const mergedSource = configs.map(c => c.source).join(' + ');
    
    return new ConfigData(mergedData, latestTimestamp, mergedSource);
  }
  
  static empty(): ConfigData {
    return new ConfigData({}, new Date(0), 'empty');
  }
  
  private getNestedValue(key: string): ConfigValue | undefined {
    const keys = key.split('.');
    let current: ConfigValue = this.data;
    
    for (const k of keys) {
      if (current === null || typeof current !== 'object' || Array.isArray(current)) {
        return undefined;
      }
      current = (current as ConfigObject)[k];
      if (current === undefined) {
        return undefined;
      }
    }
    
    return current;
  }
  
  private flattenKeys(prefix: string, obj: ConfigObject): string[] {
    const keys: string[] = [];
    
    for (const [key, value] of Object.entries(obj)) {
      const fullKey = prefix ? `${prefix}.${key}` : key;
      keys.push(fullKey);
      
      if (value && typeof value === 'object' && !Array.isArray(value)) {
        keys.push(...this.flattenKeys(fullKey, value as ConfigObject));
      }
    }
    
    return keys;
  }
  
  private static mergeObjects(target: ConfigObject, source: ConfigObject): ConfigObject {
    const result = { ...target };
    
    for (const [key, sourceValue] of Object.entries(source)) {
      const targetValue = result[key];
      
      if (
        targetValue &&
        sourceValue &&
        typeof targetValue === 'object' &&
        typeof sourceValue === 'object' &&
        !Array.isArray(targetValue) &&
        !Array.isArray(sourceValue)
      ) {
        // Deep merge for nested objects
        result[key] = this.mergeObjects(
          targetValue as ConfigObject,
          sourceValue as ConfigObject
        );
      } else if (Array.isArray(targetValue) && Array.isArray(sourceValue)) {
        // Concatenate arrays
        result[key] = [...targetValue, ...sourceValue];
      } else {
        // Right-biased merge: source overwrites target
        result[key] = sourceValue;
      }
    }
    
    return result;
  }
}

export class ConfigService {
  async fromFile(filePath: string): Promise<Result<ConfigData>> {
    try {
      const content = await fs.readFile(filePath, 'utf-8');
      const ext = filePath.split('.').pop()?.toLowerCase();
      
      let data: ConfigObject;
      
      switch (ext) {
        case 'json':
          data = JSON.parse(content);
          break;
        case 'yaml':
        case 'yml':
          data = yaml.parse(content);
          break;
        default:
          return failure(QiError.create(
            'UNSUPPORTED_FORMAT',
            `Unsupported file format: ${ext}`,
            ErrorCategory.VALIDATION,
            { filePath, format: ext }
          ));
      }
      
      return success(new ConfigData(data, new Date(), filePath));
    } catch (error) {
      return failure(QiError.fromError(error as Error, ErrorCategory.FILESYSTEM));
    }
  }
  
  fromObject(obj: ConfigObject): Result<ConfigData> {
    try {
      return success(new ConfigData(obj, new Date(), 'object'));
    } catch (error) {
      return failure(QiError.fromError(error as Error, ErrorCategory.VALIDATION));
    }
  }
  
  fromEnvironment(prefix?: string): Result<ConfigData> {
    try {
      const data: ConfigObject = {};
      
      for (const [key, value] of Object.entries(process.env)) {
        if (!prefix || key.startsWith(prefix)) {
          const cleanKey = prefix ? key.slice(prefix.length) : key;
          const nestedKey = cleanKey.toLowerCase().replace(/_/g, '.');
          this.setNestedValue(data, nestedKey, this.parseEnvValue(value || ''));
        }
      }
      
      return success(new ConfigData(data, new Date(), 'environment'));
    } catch (error) {
      return failure(QiError.fromError(error as Error, ErrorCategory.SYSTEM));
    }
  }
  
  private setNestedValue(obj: ConfigObject, path: string, value: ConfigValue): void {
    const keys = path.split('.');
    let current = obj;
    
    for (let i = 0; i < keys.length - 1; i++) {
      const key = keys[i];
      if (!(key in current) || typeof current[key] !== 'object') {
        current[key] = {};
      }
      current = current[key] as ConfigObject;
    }
    
    current[keys[keys.length - 1]] = value;
  }
  
  private parseEnvValue(value: string): ConfigValue {
    // Try to parse as number
    if (/^\d+$/.test(value)) {
      return parseInt(value, 10);
    }
    if (/^\d*\.\d+$/.test(value)) {
      return parseFloat(value);
    }
    
    // Try to parse as boolean
    if (value.toLowerCase() === 'true') return true;
    if (value.toLowerCase() === 'false') return false;
    
    // Return as string
    return value;
  }
}
```

### Step 4: Testing Setup

```typescript
// tests/base/result.test.ts
import { describe, it, expect } from 'vitest';
import { success, failure, QiError, ErrorCategory } from '../../src/base';

describe('Result<T> Monad Laws', () => {
  it('should satisfy left identity law', () => {
    const value = 42;
    const f = (x: number) => success(x * 2);
    
    const leftSide = success(value).flatMap(f);
    const rightSide = f(value);
    
    expect(leftSide.unwrap()).toBe(rightSide.unwrap());
  });
  
  it('should satisfy right identity law', () => {
    const result = success(42);
    const identity = success;
    
    expect(result.flatMap(identity).unwrap()).toBe(result.unwrap());
  });
  
  it('should satisfy associativity law', () => {
    const result = success(2);
    const f = (x: number) => success(x * 3);
    const g = (x: number) => success(x + 1);
    
    const leftSide = result.flatMap(f).flatMap(g);
    const rightSide = result.flatMap(x => f(x).flatMap(g));
    
    expect(leftSide.unwrap()).toBe(rightSide.unwrap());
  });
});
```

## Build and Test

```bash
# Install dependencies
npm install

# Build the library
npm run build

# Run tests
npm test

# Check type safety
npm run typecheck

# Format code
npm run format
```

## Integration with qi-v2-dp-actor

The TypeScript implementation serves as the foundation for the data processing actors:

```typescript
// In qi-v2-dp-actor project
import { Result, success, failure, QiError } from '@qi/qicore-foundation';

// Use foundation types in DSL implementation
export interface MarketDataReader {
  getCurrentPrice(context: DataContext): Promise<Result<MarketData<Price>>>;
}
```

This implementation provides the mathematical rigor and type safety required for production financial data processing while maintaining compatibility with the language-agnostic contracts.