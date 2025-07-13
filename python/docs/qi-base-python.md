# QiCore Base Component - Python Implementation Specification

This document provides Python-specific implementation guidance for the QiCore Base Component, mapping the language-agnostic contracts from [qi.base.contracts.md](../../docs/contracts/qi.base.contracts.md) to idiomatic Python patterns with Python 3.13+ features including experimental JIT and No-GIL mode.

## Language Mapping Overview

| Contract Operation | Python Implementation | Python 3.13+ Features |
|-------------------|------------------------|----------------------|
| `Result<T>` | `Result[T, QiError]` | Type parameter syntax |
| `andThen` | `.and_then(fn)` | JIT optimization |
| `asyncMap` | `async def -> Result[T]` | No-GIL async |
| `inspect` | `.tap(fn)` | Performance improvements |
| `ErrorCategory` | `enum.StrEnum` | Pattern matching |

## Python-Specific Result Implementation

### Core Types with Python 3.13+ Features

```python
"""QiCore Base Component - Python Implementation

Utilizes Python 3.13+ features:
- Type parameter syntax (PEP 695)
- Pattern matching improvements
- Experimental JIT compatibility
- No-GIL mode support for async operations
"""

from __future__ import annotations

import asyncio
import traceback
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import StrEnum
from typing import (
    Any, Awaitable, Callable, Dict, Generic, List, Optional, 
    Protocol, Tuple, TypeVar, Union, overload
)

# Python 3.13+ type parameter syntax
type Result[T, E = QiError] = Success[T] | Failure[E]

# Traditional syntax for compatibility
T = TypeVar('T')
E = TypeVar('E', bound='QiError')
U = TypeVar('U')

class ErrorCategory(StrEnum):
    """Error categories with retry strategies."""
    VALIDATION = "VALIDATION"
    NETWORK = "NETWORK"
    SYSTEM = "SYSTEM"
    BUSINESS = "BUSINESS"
    SECURITY = "SECURITY"
    PARSING = "PARSING"
    TIMEOUT = "TIMEOUT"
    ASYNC = "ASYNC"
    CONCURRENCY = "CONCURRENCY"
    RESOURCE = "RESOURCE"
    CONFIGURATION = "CONFIGURATION"
    SERIALIZATION = "SERIALIZATION"
    FILESYSTEM = "FILESYSTEM"
    UNKNOWN = "UNKNOWN"

@dataclass(frozen=True)
class QiError:
    """Structured error with context and chaining support."""
    code: str
    message: str
    category: ErrorCategory
    context: Dict[str, Any] = field(default_factory=dict)
    cause: Optional[QiError | Exception] = None
    timestamp: datetime = field(default_factory=datetime.now)

    @classmethod
    def create(
        cls,
        code: str,
        message: str,
        category: ErrorCategory,
        context: Optional[Dict[str, Any]] = None
    ) -> QiError:
        """Create a new QiError with optional context."""
        return cls(
            code=code,
            message=message,
            category=category,
            context=context or {},
        )

    @classmethod
    def from_exception(
        cls,
        exc: Exception,
        category: ErrorCategory = ErrorCategory.UNKNOWN
    ) -> QiError:
        """Create QiError from Python exception."""
        return cls(
            code=type(exc).__name__,
            message=str(exc),
            category=category,
            context={
                "exception_type": type(exc).__name__,
                "traceback": traceback.format_exc(),
            },
            cause=exc,
        )

    def with_context(self, additional_context: Dict[str, Any]) -> QiError:
        """Add context to error (immutable)."""
        return self.__class__(
            code=self.code,
            message=self.message,
            category=self.category,
            context={**self.context, **additional_context},
            cause=self.cause,
            timestamp=self.timestamp,
        )

    def with_cause(self, cause: QiError | Exception) -> QiError:
        """Set error cause (immutable)."""
        return self.__class__(
            code=self.code,
            message=self.message,
            category=self.category,
            context=self.context,
            cause=cause,
            timestamp=self.timestamp,
        )

    def chain(self, effect: QiError) -> QiError:
        """Chain errors (cause becomes root of effect)."""
        return effect.with_cause(self)

    def get_root_error(self) -> QiError:
        """Get root error from chain."""
        current = self
        while isinstance(current.cause, QiError):
            current = current.cause
        return current

    def has_category(self, category: ErrorCategory) -> bool:
        """Check if error has specific category."""
        return self.category == category

    def format_chain(self) -> str:
        """Format error chain for display."""
        chain_parts = []
        current: QiError | Exception | None = self
        
        while current:
            if isinstance(current, QiError):
                chain_parts.append(f"[{current.category}] {current.code}: {current.message}")
                current = current.cause
            elif isinstance(current, Exception):
                chain_parts.append(f"[WRAPPED] {type(current).__name__}: {current}")
                break
            else:
                break
        
        return " → ".join(chain_parts)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        result = {
            "code": self.code,
            "message": self.message,
            "category": self.category,
            "context": self.context,
            "timestamp": self.timestamp.isoformat(),
        }
        
        if self.cause:
            if isinstance(self.cause, QiError):
                result["cause"] = self.cause.to_dict()
            else:
                result["cause"] = str(self.cause)
        
        return result
```

### Result Base Classes

```python
class ResultProtocol[T, E](Protocol):
    """Protocol for Result types."""
    
    @abstractmethod
    def is_success(self) -> bool: ...
    
    @abstractmethod
    def is_failure(self) -> bool: ...
    
    @abstractmethod
    def map[U](self, fn: Callable[[T], U]) -> Result[U, E]: ...
    
    @abstractmethod
    def and_then[U](self, fn: Callable[[T], Result[U, E]]) -> Result[U, E]: ...

@dataclass(frozen=True)
class Success[T](Generic[T]):
    """Success case of Result."""
    value: T

    def is_success(self) -> bool:
        return True

    def is_failure(self) -> bool:
        return False

    def map[U](self, fn: Callable[[T], U]) -> Result[U, Any]:
        """Apply function to success value."""
        try:
            return Success(fn(self.value))
        except Exception as e:
            return Failure(QiError.from_exception(e))

    def and_then[U, E](self, fn: Callable[[T], Result[U, E]]) -> Result[U, E]:
        """Monadic bind operation."""
        return fn(self.value)

    def inspect(self, fn: Callable[[T], None]) -> Result[T, Any]:
        """Side effect on success value."""
        fn(self.value)
        return self

    def inspect_err(self, fn: Callable[[Any], None]) -> Result[T, Any]:
        """No-op for success."""
        return self

    def unwrap(self) -> T:
        """Extract value (safe for Success)."""
        return self.value

    def unwrap_or(self, default: T) -> T:
        """Return value (ignore default)."""
        return self.value

    def match[U](
        self,
        on_success: Callable[[T], U],
        on_failure: Callable[[Any], U]
    ) -> U:
        """Pattern matching."""
        return on_success(self.value)

@dataclass(frozen=True)  
class Failure[E](Generic[E]):
    """Failure case of Result."""
    error: E

    def is_success(self) -> bool:
        return False

    def is_failure(self) -> bool:
        return True

    def map[U](self, fn: Callable[[Any], U]) -> Result[U, E]:
        """No-op for failure."""
        return Failure(self.error)

    def and_then[U](self, fn: Callable[[Any], Result[U, E]]) -> Result[U, E]:
        """No-op for failure."""
        return Failure(self.error)

    def inspect(self, fn: Callable[[Any], None]) -> Result[Any, E]:
        """No-op for failure."""
        return self

    def inspect_err(self, fn: Callable[[E], None]) -> Result[Any, E]:
        """Side effect on error."""
        fn(self.error)
        return self

    def unwrap(self) -> Any:
        """Raise exception (unsafe)."""
        raise RuntimeError(f"Attempted to unwrap a failure: {self.error}")

    def unwrap_or[T](self, default: T) -> T:
        """Return default value."""
        return default

    def match[U](
        self,
        on_success: Callable[[Any], U],
        on_failure: Callable[[E], U]
    ) -> U:
        """Pattern matching."""
        return on_failure(self.error)
```

### Factory Functions

```python
def success[T](value: T) -> Result[T, Any]:
    """Create successful Result."""
    return Success(value)

def failure[E](error: E) -> Result[Any, E]:
    """Create failed Result."""
    return Failure(error)

def from_exception[T](fn: Callable[[], T]) -> Result[T, QiError]:
    """Create Result from potentially throwing function."""
    try:
        return success(fn())
    except Exception as e:
        return failure(QiError.from_exception(e))

def from_optional[T](
    value: Optional[T],
    error: QiError
) -> Result[T, QiError]:
    """Create Result from Optional value."""
    if value is None:
        return failure(error)
    return success(value)

# Python 3.13+ async support with No-GIL optimization
async def from_coroutine[T](
    coro: Awaitable[T]
) -> Result[T, QiError]:
    """Create Result from coroutine."""
    try:
        value = await coro
        return success(value)
    except Exception as e:
        return failure(QiError.from_exception(e, ErrorCategory.ASYNC))
```

### Collection Operations

```python
class ResultUtils:
    """Utility functions for Result collections."""

    @staticmethod
    def partition[T, E](
        results: List[Result[T, E]]
    ) -> Tuple[List[T], List[E]]:
        """Split Results into successes and failures."""
        successes = []
        failures = []
        
        for result in results:
            match result:
                case Success(value):
                    successes.append(value)
                case Failure(error):
                    failures.append(error)
        
        return successes, failures

    @staticmethod
    def sequence[T, E](results: List[Result[T, E]]) -> Result[List[T], E]:
        """Convert List[Result[T, E]] to Result[List[T], E] (fail-fast)."""
        values = []
        
        for result in results:
            match result:
                case Success(value):
                    values.append(value)
                case Failure(error):
                    return failure(error)
        
        return success(values)

    @staticmethod
    def rights[T, E](results: List[Result[T, E]]) -> List[T]:
        """Extract all successful values."""
        return [
            result.value for result in results
            if isinstance(result, Success)
        ]

    @staticmethod
    def lefts[T, E](results: List[Result[T, E]]) -> List[E]:
        """Extract all errors."""
        return [
            result.error for result in results
            if isinstance(result, Failure)
        ]

    @staticmethod
    def combine2[T, U, V, E](
        result1: Result[T, E],
        result2: Result[U, E],
        fn: Callable[[T, U], V]
    ) -> Result[V, E]:
        """Combine two Results with function."""
        match (result1, result2):
            case (Success(x), Success(y)):
                return success(fn(x, y))
            case (Failure(e), _):
                return failure(e)
            case (_, Failure(e)):
                return failure(e)

    # Python 3.13+ async collection operations with No-GIL support
    @staticmethod
    async def async_sequence[T](
        async_results: List[Awaitable[Result[T, QiError]]]
    ) -> Result[List[T], QiError]:
        """Async sequence with concurrent execution."""
        results = await asyncio.gather(*async_results, return_exceptions=True)
        
        values = []
        for result in results:
            if isinstance(result, Exception):
                return failure(QiError.from_exception(result, ErrorCategory.ASYNC))
            
            match result:
                case Success(value):
                    values.append(value)
                case Failure(error):
                    return failure(error)
        
        return success(values)

    @staticmethod
    async def async_map[T, U](
        fn: Callable[[T], Awaitable[U]],
        result: Result[T, QiError]
    ) -> Result[U, QiError]:
        """Async map operation."""
        match result:
            case Success(value):
                try:
                    mapped_value = await fn(value)
                    return success(mapped_value)
                except Exception as e:
                    return failure(QiError.from_exception(e, ErrorCategory.ASYNC))
            case Failure(error):
                return failure(error)
```

### Pattern Matching Support (Python 3.13+)

```python
# Enhanced pattern matching for Results
def handle_result[T, E](result: Result[T, E]) -> str:
    """Demonstrate pattern matching with Results."""
    match result:
        case Success(value) if isinstance(value, str):
            return f"String success: {value}"
        case Success(value) if isinstance(value, int):
            return f"Integer success: {value}"
        case Success(value):
            return f"Other success: {value}"
        case Failure(error) if hasattr(error, 'category'):
            return f"Categorized error: {error.category} - {error}"
        case Failure(error):
            return f"General error: {error}"

# Error recovery patterns
def recover_from_error[T](
    result: Result[T, QiError],
    recovery_strategies: Dict[ErrorCategory, Callable[[], T]]
) -> T:
    """Recover from errors based on category."""
    match result:
        case Success(value):
            return value
        case Failure(error) if error.category in recovery_strategies:
            return recovery_strategies[error.category]()
        case Failure(error):
            raise RuntimeError(f"Unrecoverable error: {error.format_chain()}")
```

### JIT-Optimized Operations (Python 3.13+)

```python
# JIT-friendly Result operations for performance
def batch_map_numeric[T: (int, float)](
    fn: Callable[[T], T],
    results: List[Result[T, QiError]]
) -> List[Result[T, QiError]]:
    """Batch map operation optimized for JIT compilation."""
    # Simple loop structure that JIT can optimize
    output = []
    for result in results:
        if result.is_success():
            try:
                new_value = fn(result.unwrap())
                output.append(success(new_value))
            except Exception as e:
                output.append(failure(QiError.from_exception(e)))
        else:
            output.append(result)
    return output

# Performance-critical path for numerical computations
def compute_pipeline(values: List[float]) -> List[Result[float, QiError]]:
    """JIT-optimized computation pipeline."""
    results = []
    for value in values:
        if value < 0:
            results.append(failure(QiError.create(
                "NEGATIVE_VALUE", 
                "Negative values not allowed",
                ErrorCategory.VALIDATION
            )))
        else:
            # Simple mathematical operations that JIT can optimize
            computed = value * 2 + 1
            results.append(success(computed))
    return results
```

### Integration with Python Ecosystem

```python
# asyncio integration for modern async patterns
class AsyncResultContext:
    """Context manager for async Result operations."""
    
    async def __aenter__(self) -> AsyncResultContext:
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        if exc_type:
            # Convert exception to Result for consistent error handling
            pass

# Pydantic integration for validation
try:
    from pydantic import BaseModel, Field
    
    class ResultModel[T](BaseModel, Generic[T]):
        """Pydantic model for Result serialization."""
        kind: str = Field(..., pattern="^(success|failure)$")
        value: Optional[T] = None
        error: Optional[Dict[str, Any]] = None
        
        @classmethod
        def from_result(cls, result: Result[T, QiError]) -> ResultModel[T]:
            match result:
                case Success(value):
                    return cls(kind="success", value=value)
                case Failure(error):
                    return cls(kind="failure", error=error.to_dict())
    
except ImportError:
    # Graceful degradation if Pydantic not available
    pass

# dataclasses integration for structured data
from dataclasses import asdict

def result_to_dict[T, E](result: Result[T, E]) -> Dict[str, Any]:
    """Convert Result to dictionary for JSON serialization."""
    match result:
        case Success(value):
            return {
                "kind": "success",
                "value": asdict(value) if hasattr(value, '__dataclass_fields__') else value
            }
        case Failure(error):
            return {
                "kind": "failure", 
                "error": error.to_dict() if hasattr(error, 'to_dict') else str(error)
            }
```

## Usage Examples

See [examples/](./examples/) directory for complete Python usage patterns including:
- Basic Result operations with pattern matching
- Async/await integration with No-GIL mode
- Error handling and recovery strategies  
- Integration with popular Python libraries (FastAPI, Pydantic, etc.)
- JIT-optimized numerical computations

## Python Compatibility

- **Python 3.13+**: Full support including JIT, No-GIL mode, enhanced pattern matching
- **Python 3.11+**: Core functionality without JIT optimizations
- **asyncio**: Modern async/await patterns with concurrent execution
- **Type Checking**: mypy and pyright compatibility

This specification ensures Python developers can use QiCore idiomatically with modern Python features while maintaining full compliance with the language-agnostic behavioral contracts.