# QiCore Base Component - C++ Implementation Specification

This document provides C++-specific implementation guidance for the QiCore Base Component, mapping the language-agnostic contracts from [qi.v4.base.contracts.md](../../docs/contracts/qi.v4.base.contracts.md) to idiomatic C++ patterns with C++23/26 features including modules, `std::expected`, and upcoming reflection.

## Language Mapping Overview

| Contract Operation | C++ Implementation | C++23/26 Features |
|-------------------|-------------------|-------------------|
| `Result<T>` | `std::expected<T, QiError>` | Enhanced expected |
| `andThen` | `.and_then(fn)` | Monadic operations |
| `asyncMap` | `std::future<std::expected<T>>` | Execution policies |
| `inspect` | `.inspect(fn)` | Function composition |
| `ErrorCategory` | `enum class ErrorCategory` | Strong typing |

## C++-Specific Result Implementation

### Core Types with C++23 Features

```cpp
// qi_base.cppm - C++23 module interface
module;

#include <expected>
#include <string>
#include <string_view>
#include <unordered_map>
#include <chrono>
#include <concepts>
#include <ranges>
#include <functional>
#include <print>  // C++23 std::print

export module qi.base;

namespace qi::base {

// C++23 enum class with underlying type
export enum class ErrorCategory : std::uint8_t {
    VALIDATION,
    NETWORK,
    SYSTEM,
    BUSINESS,
    SECURITY,
    PARSING,
    TIMEOUT,
    ASYNC,
    CONCURRENCY,
    RESOURCE,
    CONFIGURATION,
    SERIALIZATION,
    FILESYSTEM,
    UNKNOWN
};

// C++23 string conversion for ErrorCategory
export constexpr std::string_view to_string(ErrorCategory category) noexcept {
    switch (category) {
        case ErrorCategory::VALIDATION: return "VALIDATION";
        case ErrorCategory::NETWORK: return "NETWORK";
        case ErrorCategory::SYSTEM: return "SYSTEM";
        case ErrorCategory::BUSINESS: return "BUSINESS";
        case ErrorCategory::SECURITY: return "SECURITY";
        case ErrorCategory::PARSING: return "PARSING";
        case ErrorCategory::TIMEOUT: return "TIMEOUT";
        case ErrorCategory::ASYNC: return "ASYNC";
        case ErrorCategory::CONCURRENCY: return "CONCURRENCY";
        case ErrorCategory::RESOURCE: return "RESOURCE";
        case ErrorCategory::CONFIGURATION: return "CONFIGURATION";
        case ErrorCategory::SERIALIZATION: return "SERIALIZATION";
        case ErrorCategory::FILESYSTEM: return "FILESYSTEM";
        case ErrorCategory::UNKNOWN: return "UNKNOWN";
    }
    return "UNKNOWN";
}

// Structured error type with modern C++ features
export class QiError {
public:
    using Context = std::unordered_map<std::string, std::string>;
    using TimePoint = std::chrono::system_clock::time_point;

private:
    std::string code_;
    std::string message_;
    ErrorCategory category_;
    Context context_;
    std::unique_ptr<QiError> cause_;
    TimePoint timestamp_;

public:
    // C++23 constructor with designated initializers support
    QiError(std::string code, 
            std::string message,
            ErrorCategory category,
            Context context = {},
            std::unique_ptr<QiError> cause = nullptr,
            TimePoint timestamp = std::chrono::system_clock::now())
        : code_(std::move(code))
        , message_(std::move(message))
        , category_(category)
        , context_(std::move(context))
        , cause_(std::move(cause))
        , timestamp_(timestamp) {}

    // Factory methods
    static QiError create(std::string_view code,
                         std::string_view message,
                         ErrorCategory category,
                         Context context = {}) {
        return QiError{std::string(code), std::string(message), category, std::move(context)};
    }

    static QiError from_exception(const std::exception& ex, 
                                 ErrorCategory category = ErrorCategory::UNKNOWN) {
        return QiError{
            "EXCEPTION",
            ex.what(),
            category,
            {{"exception_type", typeid(ex).name()}}
        };
    }

    // Accessors
    [[nodiscard]] const std::string& code() const noexcept { return code_; }
    [[nodiscard]] const std::string& message() const noexcept { return message_; }
    [[nodiscard]] ErrorCategory category() const noexcept { return category_; }
    [[nodiscard]] const Context& context() const noexcept { return context_; }
    [[nodiscard]] const QiError* cause() const noexcept { return cause_.get(); }
    [[nodiscard]] TimePoint timestamp() const noexcept { return timestamp_; }

    // Immutable transformations
    [[nodiscard]] QiError with_context(Context additional_context) const {
        auto new_context = context_;
        new_context.merge(additional_context);
        return QiError{code_, message_, category_, std::move(new_context), 
                      cause_ ? std::make_unique<QiError>(*cause_) : nullptr, timestamp_};
    }

    [[nodiscard]] QiError with_cause(QiError cause) const {
        return QiError{code_, message_, category_, context_, 
                      std::make_unique<QiError>(std::move(cause)), timestamp_};
    }

    // Error chain operations
    [[nodiscard]] QiError chain(QiError effect) const {
        return effect.with_cause(*this);
    }

    [[nodiscard]] const QiError& get_root_error() const noexcept {
        const QiError* current = this;
        while (current->cause_) {
            current = current->cause_.get();
        }
        return *current;
    }

    [[nodiscard]] bool has_category(ErrorCategory cat) const noexcept {
        return category_ == cat;
    }

    // C++23 std::print integration
    [[nodiscard]] std::string format_chain() const {
        std::string result;
        const QiError* current = this;
        bool first = true;
        
        while (current) {
            if (!first) result += " → ";
            result += std::format("[{}] {}: {}", 
                                to_string(current->category_),
                                current->code_,
                                current->message_);
            current = current->cause_.get();
            first = false;
        }
        return result;
    }

    // Copy and move operations
    QiError(const QiError& other) 
        : code_(other.code_)
        , message_(other.message_)
        , category_(other.category_)
        , context_(other.context_)
        , cause_(other.cause_ ? std::make_unique<QiError>(*other.cause_) : nullptr)
        , timestamp_(other.timestamp_) {}

    QiError& operator=(const QiError& other) {
        if (this != &other) {
            code_ = other.code_;
            message_ = other.message_;
            category_ = other.category_;
            context_ = other.context_;
            cause_ = other.cause_ ? std::make_unique<QiError>(*other.cause_) : nullptr;
            timestamp_ = other.timestamp_;
        }
        return *this;
    }

    QiError(QiError&&) = default;
    QiError& operator=(QiError&&) = default;
};

// Result type alias using std::expected (C++23)
export template<typename T>
using Result = std::expected<T, QiError>;

// Factory functions
export template<typename T>
constexpr Result<T> success(T&& value) noexcept(std::is_nothrow_constructible_v<T>) {
    return std::expected<T, QiError>{std::forward<T>(value)};
}

export template<typename T = void>
constexpr Result<T> failure(QiError error) noexcept {
    return std::unexpected{std::move(error)};
}

// C++23 concepts for Result operations
export template<typename T>
concept ResultType = requires(T t) {
    typename T::value_type;
    typename T::error_type;
    { t.has_value() } -> std::convertible_to<bool>;
    { t.value() } -> std::convertible_to<typename T::value_type>;
    { t.error() } -> std::convertible_to<typename T::error_type>;
};

} // namespace qi::base
```

### Monadic Operations with C++23 std::expected

```cpp
// qi_base_operations.cppm - Extended operations module
export module qi.base.operations;

import qi.base;
import <functional>;
import <future>;
import <ranges>;

namespace qi::base {

// Monadic operations for Result (leveraging std::expected's and_then)
export template<typename T, std::invocable<T> F>
    requires ResultType<std::invoke_result_t<F, T>>
auto and_then(const Result<T>& result, F&& func) -> std::invoke_result_t<F, T> {
    return result.and_then(std::forward<F>(func));
}

// Rust-style inspect operations
export template<typename T, std::invocable<const T&> F>
    requires std::same_as<void, std::invoke_result_t<F, const T&>>
auto inspect(Result<T> result, F&& func) -> Result<T> {
    if (result.has_value()) {
        std::invoke(std::forward<F>(func), result.value());
    }
    return result;
}

export template<typename T, std::invocable<const QiError&> F>
    requires std::same_as<void, std::invoke_result_t<F, const QiError&>>
auto inspect_err(Result<T> result, F&& func) -> Result<T> {
    if (!result.has_value()) {
        std::invoke(std::forward<F>(func), result.error());
    }
    return result;
}

// Collection operations using C++23 ranges
export template<std::ranges::range R>
    requires ResultType<std::ranges::range_value_t<R>>
auto partition(R&& results) {
    using ValueType = typename std::ranges::range_value_t<R>::value_type;
    using ErrorType = typename std::ranges::range_value_t<R>::error_type;
    
    std::vector<ValueType> successes;
    std::vector<ErrorType> failures;
    
    for (auto&& result : results) {
        if (result.has_value()) {
            successes.emplace_back(std::move(result.value()));
        } else {
            failures.emplace_back(std::move(result.error()));
        }
    }
    
    return std::make_pair(std::move(successes), std::move(failures));
}

export template<std::ranges::range R>
    requires ResultType<std::ranges::range_value_t<R>>
auto sequence(R&& results) -> Result<std::vector<typename std::ranges::range_value_t<R>::value_type>> {
    using ValueType = typename std::ranges::range_value_t<R>::value_type;
    
    std::vector<ValueType> values;
    values.reserve(std::ranges::size(results));
    
    for (auto&& result : results) {
        if (!result.has_value()) {
            return failure<std::vector<ValueType>>(std::move(result.error()));
        }
        values.emplace_back(std::move(result.value()));
    }
    
    return success(std::move(values));
}

// Rights and lefts using ranges views (C++23)
export template<std::ranges::range R>
    requires ResultType<std::ranges::range_value_t<R>>
auto rights(R&& results) {
    return results 
         | std::views::filter([](const auto& r) { return r.has_value(); })
         | std::views::transform([](auto&& r) { return std::forward<decltype(r)>(r).value(); });
}

export template<std::ranges::range R>
    requires ResultType<std::ranges::range_value_t<R>>
auto lefts(R&& results) {
    return results 
         | std::views::filter([](const auto& r) { return !r.has_value(); })
         | std::views::transform([](auto&& r) { return std::forward<decltype(r)>(r).error(); });
}

// Combine operations
export template<typename T, typename U, std::invocable<T, U> F>
auto combine2(Result<T> result1, Result<U> result2, F&& func) -> Result<std::invoke_result_t<F, T, U>> {
    if (!result1.has_value()) {
        return failure<std::invoke_result_t<F, T, U>>(std::move(result1.error()));
    }
    if (!result2.has_value()) {
        return failure<std::invoke_result_t<F, T, U>>(std::move(result2.error()));
    }
    
    return success(std::invoke(std::forward<F>(func), 
                              std::move(result1.value()), 
                              std::move(result2.value())));
}

} // namespace qi::base
```

### Async Operations with std::execution (C++26)

```cpp
// qi_base_async.cppm - Async operations for future C++26
export module qi.base.async;

import qi.base;
import <execution>;  // C++26 std::execution
import <future>;
import <coroutine>;

namespace qi::base {

// Future-based async operations
export template<typename T>
using AsyncResult = std::future<Result<T>>;

export template<typename T, std::invocable<T> F>
    requires std::same_as<std::invoke_result_t<F, T>, std::future<typename Result<T>::value_type>>
auto async_map(Result<T> result, F&& func) -> std::future<Result<std::invoke_result_t<F, T>::value_type>> {
    if (!result.has_value()) {
        std::promise<Result<std::invoke_result_t<F, T>::value_type>> promise;
        promise.set_value(failure<std::invoke_result_t<F, T>::value_type>(std::move(result.error())));
        return promise.get_future();
    }
    
    return std::async(std::launch::async, [value = std::move(result.value()), func = std::forward<F>(func)]() mutable {
        try {
            auto future_result = func(std::move(value));
            return success(future_result.get());
        } catch (const std::exception& e) {
            return failure<std::invoke_result_t<F, T>::value_type>(
                QiError::from_exception(e, ErrorCategory::ASYNC));
        }
    });
}

export template<typename T, std::invocable<T> F>
    requires std::same_as<std::invoke_result_t<F, T>, std::future<Result<typename Result<T>::value_type>>>
auto async_and_then(Result<T> result, F&& func) -> std::future<Result<std::invoke_result_t<F, T>::value_type::value_type>> {
    if (!result.has_value()) {
        std::promise<Result<std::invoke_result_t<F, T>::value_type::value_type>> promise;
        promise.set_value(failure<std::invoke_result_t<F, T>::value_type::value_type>(std::move(result.error())));
        return promise.get_future();
    }
    
    return std::async(std::launch::async, [value = std::move(result.value()), func = std::forward<F>(func)]() mutable {
        try {
            auto future_result = func(std::move(value));
            return future_result.get();
        } catch (const std::exception& e) {
            return failure<std::invoke_result_t<F, T>::value_type::value_type>(
                QiError::from_exception(e, ErrorCategory::ASYNC));
        }
    });
}

// C++26 std::execution integration (when available)
#if __cpp_lib_execution >= 202400L  // C++26 feature test

export template<std::execution::scheduler S, typename T>
auto schedule_result(S&& scheduler, Result<T> result) {
    return std::execution::schedule(std::forward<S>(scheduler))
         | std::execution::then([result = std::move(result)](auto) mutable {
             return std::move(result);
           });
}

export template<std::execution::scheduler S, std::ranges::range R>
    requires ResultType<std::ranges::range_value_t<R>>
auto async_sequence_execution(S&& scheduler, R&& results) {
    using ValueType = typename std::ranges::range_value_t<R>::value_type;
    
    return std::execution::schedule(std::forward<S>(scheduler))
         | std::execution::then([results = std::forward<R>(results)](auto) mutable {
             return sequence(std::forward<R>(results));
           });
}

#endif // C++26 execution

} // namespace qi::base
```

### SIMD and Performance Optimizations (C++26)

```cpp
// qi_base_simd.cppm - SIMD optimizations for numeric Results
export module qi.base.simd;

import qi.base;
import <experimental/simd>;  // C++26 SIMD library
import <vector>;
import <algorithm>;

namespace qi::base {

#if __cpp_lib_experimental_parallel_simd >= 202400L  // C++26 SIMD

export template<typename T>
    requires std::is_arithmetic_v<T>
auto batch_map_numeric(const std::vector<Result<T>>& results, 
                      std::function<T(T)> func) -> std::vector<Result<T>> {
    std::vector<Result<T>> output;
    output.reserve(results.size());
    
    // Extract values for SIMD processing
    std::vector<T> values;
    std::vector<std::size_t> success_indices;
    
    for (std::size_t i = 0; i < results.size(); ++i) {
        if (results[i].has_value()) {
            values.push_back(results[i].value());
            success_indices.push_back(i);
        }
    }
    
    // SIMD processing of values
    using simd_t = std::experimental::native_simd<T>;
    constexpr std::size_t simd_size = simd_t::size();
    
    std::vector<T> processed_values(values.size());
    
    std::size_t i = 0;
    for (; i + simd_size <= values.size(); i += simd_size) {
        simd_t simd_vals;
        simd_vals.copy_from(values.data() + i, std::experimental::element_aligned);
        
        // Apply function element-wise (requires SIMD-compatible function)
        for (std::size_t j = 0; j < simd_size; ++j) {
            simd_vals[j] = func(simd_vals[j]);
        }
        
        simd_vals.copy_to(processed_values.data() + i, std::experimental::element_aligned);
    }
    
    // Handle remaining elements
    for (; i < values.size(); ++i) {
        processed_values[i] = func(values[i]);
    }
    
    // Reconstruct results
    std::size_t value_idx = 0;
    for (std::size_t i = 0; i < results.size(); ++i) {
        if (results[i].has_value()) {
            output.emplace_back(success(processed_values[value_idx++]));
        } else {
            output.emplace_back(results[i]);
        }
    }
    
    return output;
}

#endif // C++26 SIMD

} // namespace qi::base
```

### Integration with Modern C++ Libraries

```cpp
// qi_base_integration.cppm - Integration with standard library
export module qi.base.integration;

import qi.base;
import <iostream>;
import <format>;  // C++23
import <print>;   // C++23

namespace qi::base {

// C++23 std::print integration
export template<typename T>
void print_result(const Result<T>& result) {
    if (result.has_value()) {
        std::print("Success: {}\n", result.value());
    } else {
        std::print("Failure: {}\n", result.error().format_chain());
    }
}

// Formatting support for Results
export template<typename T>
struct std::formatter<qi::base::Result<T>> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }
    
    auto format(const qi::base::Result<T>& result, std::format_context& ctx) const {
        if (result.has_value()) {
            return std::format_to(ctx.out(), "Success({})", result.value());
        } else {
            return std::format_to(ctx.out(), "Failure({})", result.error().format_chain());
        }
    }
};

// Stream operators
export template<typename T>
std::ostream& operator<<(std::ostream& os, const Result<T>& result) {
    if (result.has_value()) {
        return os << "Success(" << result.value() << ")";
    } else {
        return os << "Failure(" << result.error().format_chain() << ")";
    }
}

// Exception conversion utilities
export template<typename T>
Result<T> from_exception_ptr(std::exception_ptr ptr) noexcept {
    try {
        if (ptr) {
            std::rethrow_exception(ptr);
        }
        // This shouldn't happen if ptr is valid
        return failure<T>(QiError::create("NULL_EXCEPTION_PTR", 
                                         "Null exception pointer", 
                                         ErrorCategory::UNKNOWN));
    } catch (const std::exception& e) {
        return failure<T>(QiError::from_exception(e));
    } catch (...) {
        return failure<T>(QiError::create("UNKNOWN_EXCEPTION", 
                                         "Unknown exception type", 
                                         ErrorCategory::UNKNOWN));
    }
}

} // namespace qi::base
```

### C++26 Reflection Integration (Future)

```cpp
// qi_base_reflection.cppm - Future C++26 reflection support
export module qi.base.reflection;

#if __cpp_reflection >= 202600L  // C++26 reflection

import qi.base;
import <experimental/reflect>;

namespace qi::base {

// Automatic error serialization using reflection
export template<typename T>
    requires std::experimental::reflect::Record<T>
auto serialize_error_context(const T& obj) -> QiError::Context {
    QiError::Context context;
    
    using refl = std::experimental::reflect;
    constexpr auto members = refl::members_of(refl::type_of(obj));
    
    []<std::size_t... Is>(std::index_sequence<Is...>, const T& obj, auto& context) {
        ((context[std::string(refl::name_of(std::get<Is>(members)))] = 
          std::format("{}", refl::value_of(std::get<Is>(members), obj))), ...);
    }(std::make_index_sequence<members.size()>{}, obj, context);
    
    return context;
}

// Automatic QiError construction from structured data
export template<typename T>
    requires std::experimental::reflect::Record<T>
QiError create_contextual_error(std::string_view code,
                               std::string_view message,
                               ErrorCategory category,
                               const T& context_obj) {
    return QiError::create(code, message, category, serialize_error_context(context_obj));
}

} // namespace qi::base

#endif // C++26 reflection
```

## Usage Examples

See [examples/](./examples/) directory for complete C++ usage patterns including:
- Basic Result operations with modern C++
- Async operations with futures and coroutines
- SIMD-optimized batch processing
- Integration with standard library components
- Module usage patterns

## C++ Compatibility

- **C++23**: Core functionality with modules, `std::expected`, `std::print`
- **C++26**: Enhanced with reflection, advanced execution, SIMD optimizations  
- **Compilers**: GCC 14+, Clang 17+, MSVC 19.40+
- **CMake**: 3.28+ for module support

This specification ensures C++ developers can use QiCore idiomatically with cutting-edge C++ features while maintaining full compliance with the language-agnostic behavioral contracts.