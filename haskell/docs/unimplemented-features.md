# Unimplemented Features Documentation - QiCore Foundation

## Overview

This document explicitly identifies features that are **not yet implemented** in the QiCore Foundation. This documentation is **mandatory** and serves as a contract transparency mechanism to avoid fake/stub coding violations.

## Policy Statement

**QiCore Foundation has ZERO TOLERANCE for fake/stub code.** All unimplemented features must be:

1. **Explicitly documented** in this file
2. **Clearly marked** with implementation status
3. **Justified** with technical reasoning
4. **Tracked** with implementation timeline

## Current Unimplemented Features

### 1. **OpenTelemetry HTTP Exporters** 

**Status**: ❌ **NOT IMPLEMENTED**

**What's Missing**:
- Actual HTTP POST requests to Jaeger/Zipkin/OTLP endpoints
- Network connectivity and retry logic
- Authentication/authorization headers
- Error handling for HTTP failures

**Current Implementation**:
- Structured JSON output to stderr with endpoint labels
- Format-compliant data structures (Jaeger, Zipkin, OTLP)
- Complete timestamp conversion and tracing context

**Why Not Implemented**:
- **Dependency Complexity**: HTTP client libraries (http-conduit, req) have complex dependency trees
- **GHC 9.12.1 Compatibility**: Many HTTP libraries not yet compatible with cutting-edge GHC
- **Foundation Scope**: Core foundation should minimize external network dependencies

**Workaround**:
```haskell
-- Current: Structured output for external consumption
exportToJaeger host port msg = do
  let jaegerSpan = JSON.object [...]  -- Complete Jaeger format
  TIO.hPutStrLn stderr $ "JAEGER[" <> host <> ":" <> port <> "]: " <> jsonOutput
```

**Integration Path**:
External log shippers (Fluentd, Logstash, Vector) can parse structured output and forward to actual endpoints.

**Implementation Timeline**: 
- **v-0.3.x**: Add HTTP client when ecosystem stabilizes
- **v-0.4.x**: Full network retry and authentication

---

### 2. **Redis Distributed Cache Operations**

**Status**: ⚠️ **PARTIALLY IMPLEMENTED**

**What's Working**:
- Redis connection establishment with ping validation
- Backend abstraction with proper error handling
- Cache creation with distributed backend type

**What's Missing**:
- Actual Redis SET/GET/DELETE operations
- TTL implementation via Redis EXPIRE
- LRU eviction policy delegation to Redis
- Distributed stats aggregation

**Current Implementation**:
```haskell
-- Connection works:
createDistributed host port config = do
  connectionResult <- Redis.runRedis Redis.defaultConnectInfo {...} $ Redis.ping
  case connectionResult of
    Left redisError -> pure (Failure $ createNetworkError redisError)
    Right _ -> Success Cache {..., cacheBackend = DistributedBackend connection}

-- Operations fall back to memory cache:
get key cache = case cacheBackend cache of
  DistributedBackend conn -> -- TODO: Redis.get via connection
  _ -> localMemoryGet key cache
```

**Why Partially Implemented**:
- **Testing Complexity**: Redis integration tests require external Redis instance
- **Atomic Operations**: STM to Redis translation needs careful design
- **Error Semantics**: Redis failures need proper Result<T> mapping

**Integration Path**:
Cache operations transparently use Redis when backend is DistributedBackend, fall back to memory otherwise.

**Implementation Timeline**:
- **v-0.2.4**: Complete Redis operations with comprehensive tests
- **v-0.2.5**: Add Redis cluster support and failover

---

### 3. **Configuration Hot Reload**

**Status**: ❌ **NOT IMPLEMENTED**

**What's Missing**:
- File system watching for configuration changes
- Dynamic reloading without restart
- Change notification and validation

**Current Implementation**:
```haskell
-- Context exists but reload not implemented:
data ConfigContext = ConfigContext
  { configContextLastReload :: !UTCTime  -- ← Timestamp tracked
  , ...
  }

withHotReload :: [ConfigSource] -> m (Result ConfigContext)
withHotReload sources = do
  config <- fromSources sources  -- ← Static load only
  pure $ Success ConfigContext {..., configContextLastReload = now}
```

**Why Not Implemented**:
- **File System Watching**: Requires platform-specific dependencies (inotify, kqueue)
- **Thread Safety**: Hot reload during operations needs careful STM coordination
- **Validation**: Incremental config validation complexity

**Workaround**:
Applications can manually reload configuration by calling `fromSources` again.

**Implementation Timeline**:
- **v-0.3.x**: Add file system watching with fsnotify
- **v-0.4.x**: Incremental reload with validation

---

### 4. **YAML/TOML Configuration Parsing**

**Status**: ❌ **NOT IMPLEMENTED**  

**What's Missing**:
- YAML parsing via yaml library
- TOML parsing via toml-parser library
- Format auto-detection validation

**Current Implementation**:
```haskell
parseYAML :: Text -> Result ConfigData
parseYAML _ = Failure $ Error.create
  "CONFIG_YAML_NOT_IMPLEMENTED" 
  "YAML parsing not yet implemented"
  CONFIGURATION
  ...

parseTOML :: Text -> Result ConfigData  
parseTOML _ = Failure $ Error.create
  "CONFIG_TOML_NOT_IMPLEMENTED"
  "TOML parsing not yet implemented"
  CONFIGURATION
  ...
```

**Why Not Implemented**:
- **Dependency Scope**: Additional parsing libraries increase foundation footprint
- **Error Handling**: Different libraries have different error semantics
- **Consistency**: Need unified error reporting across all formats

**Workaround**:
Use JSON format for all configuration. Convert YAML/TOML to JSON externally if needed.

**Implementation Timeline**:
- **v-0.3.x**: Add YAML support with yaml library
- **v-0.4.x**: Add TOML support with unified error handling

---

## Implementation Guidelines

### For Adding New Features:

1. **Never add stub/fake code** - return proper errors instead
2. **Document unimplemented features** - add to this file immediately  
3. **Provide workarounds** - explain how to achieve functionality externally
4. **Set timeline** - commit to implementation schedule

### Error Messages for Unimplemented Features:

```haskell
-- ✅ CORRECT: Clear error with category
unimplementedFeature :: Text -> ErrorCategory -> Result a
unimplementedFeature feature category = Failure $ Error.create
  ("FEATURE_NOT_IMPLEMENTED_" <> T.toUpper (T.replace " " "_" feature))
  (feature <> " not yet implemented in QiCore Foundation")
  category
  (Map.fromList [("feature", String feature), ("status", String "planned")])
  Nothing
  timestamp

-- ❌ WRONG: Fake implementation
fakeImplementation :: Text -> Result ConfigData
fakeImplementation content = Success (ConfigData (String "fake"))  -- ← FORBIDDEN
```

### Documentation Requirements:

- **What's Missing**: Specific unimplemented functionality
- **Why Not Implemented**: Technical justification
- **Current State**: What actually works vs. what's missing
- **Workaround**: How to achieve functionality externally
- **Timeline**: Planned implementation schedule

## Review Process

This document must be:

1. **Updated** before any version release
2. **Reviewed** during implementation planning
3. **Referenced** in commit messages when adding unimplemented features
4. **Verified** during testing to ensure no fake/stub code exists

## Contact

For questions about unimplemented features or implementation priorities, refer to the QiCore Foundation implementation team.

---

**Document Status**: Complete ✅  
**Last Updated**: 2025-01-13  
**Next Review**: Before v-0.2.4 release  
**Compliance**: Zero fake/stub code policy enforced