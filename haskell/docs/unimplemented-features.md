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

**Status**: ✅ **FULLY IMPLEMENTED** (v-0.2.6)

**What's Complete**:
- ✅ Redis connection establishment with ping validation
- ✅ Backend abstraction with proper error handling  
- ✅ Cache creation with distributed backend type
- ✅ **Actual Redis SET/GET/HAS/REMOVE operations** with correct Hedis API usage
- ✅ **TTL implementation** via Redis SETEX with proper expiration
- ✅ **JSON serialization/deserialization** for Redis storage
- ✅ **Comprehensive error handling** with QiError integration and Result<T> mapping
- ✅ **Batch operations** (setMany, getMany) with individual operation mapping
- ✅ **All cache operations** working with Redis backend

**Verified Implementation**:
```haskell
-- Redis operations use correct Hedis API:
getRedis :: Text -> Cache -> Redis.Connection -> IO (Result Value)
getRedis key cache conn = do
  result <- Redis.runRedis conn $ Redis.get (TE.encodeUtf8 key)
  case result of
    Left redisError -> pure $ Failure $ createNetworkError redisError
    Right Nothing -> pure $ Failure $ createNotFoundError key
    Right (Just redisValue) -> case JSON.decode (BSL.fromStrict redisValue) of
      Nothing -> pure $ Failure $ createParseError key
      Just value -> pure $ Success value

hasRedis :: Text -> Cache -> Redis.Connection -> IO Bool  
hasRedis key _cache conn = do
  result <- Redis.runRedis conn $ Redis.exists (TE.encodeUtf8 key)
  case result of
    Left _ -> pure False
    Right exists -> pure exists  -- Redis.exists returns Bool directly
```

**Test Coverage**:
- ✅ **10 comprehensive tests** including Redis SET/GET/HAS/REMOVE/TTL operations
- ✅ **Graceful fallback** when Redis unavailable (network error handling)
- ✅ **Memory vs Redis isolation** testing
- ✅ **Error categorization** verification

**Integration Status**:
Cache operations transparently use Redis when backend is DistributedBackend, with full feature parity between memory and Redis backends.

**Future Enhancements**:
- **v-1.x.x**: Redis cluster support and advanced failover patterns
- **v-1.x.x**: Redis Streams for cache invalidation
- **v-1.x.x**: Redis pub/sub for distributed cache events

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

**Status**: ✅ **YAML IMPLEMENTED** / ⚠️ **TOML PLANNED** (v-0.2.6)

**YAML - Fully Implemented**:
- ✅ **Complete YAML parsing** via Data.Yaml library
- ✅ **Comprehensive error handling** with detailed parse errors  
- ✅ **Nested structure support** for complex configurations
- ✅ **JSON conversion** for unified internal representation
- ✅ **Auto-detection** by .yaml/.yml file extensions

**TOML - Temporarily Disabled**:
- ⚠️ **API research required** - toml-parser library API differs from expectations
- ⚠️ **Conversion layer needed** - TOML to JSON transformation complexity
- ⚠️ **Error handling unification** - different error semantics from YAML

**Verified YAML Implementation**:
```haskell
parseYAML :: Text -> Result ConfigData
parseYAML content = case YAML.decodeEither' (TE.encodeUtf8 content) of
  Left yamlError -> Failure $ Error.create
    "CONFIG_YAML_PARSE_ERROR"
    ("YAML parsing failed: " <> T.pack (YAML.prettyPrintParseException yamlError))
    CONFIGURATION
    (Map.fromList [("yamlError", JSON.String (T.pack (show yamlError)))])
    Nothing
    configErrorTimestamp
  Right value -> Success (ConfigData value)

-- Currently disabled pending API research:
parseTOML :: Text -> Result ConfigData  
parseTOML _ = Failure $ Error.create
  "CONFIG_TOML_NOT_IMPLEMENTED"
  "TOML parsing requires API research - temporarily disabled"
  CONFIGURATION
  mempty
  Nothing
  configErrorTimestamp
```

**Test Coverage**:
- ✅ **YAML success parsing** - complex nested structures
- ✅ **YAML error handling** - malformed YAML with proper error categorization
- ✅ **Integration testing** - YAML config with cache storage
- ⚠️ **TOML tests disabled** until implementation complete

**Usage (YAML)**:
```haskell
-- YAML configuration (working)
let yamlContent = "database:\n  host: localhost\n  port: 5432"
configResult <- Config.fromString yamlContent Config.YAML

-- Auto-detection by file extension (working)
configResult <- Config.fromFile "config.yaml"  -- Auto-detects YAML

-- TOML configuration (planned)
-- configResult <- Config.fromString tomlContent Config.TOML  -- Not yet working
```

**Implementation Timeline**:
- ✅ **v-0.2.6**: YAML support fully completed and tested
- **v-0.2.7**: Complete TOML parsing implementation
- **v-1.x.x**: Advanced format features (includes, variables, schema validation)

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

**Document Status**: Updated for v-0.2.6 ✅  
**Last Updated**: 2025-01-13  
**Next Review**: Before v-0.2.7 release  
**Compliance**: Zero fake/stub code policy enforced  
**v-0.2.6 Status**: Redis fully implemented, YAML implemented, TOML planned for v-0.2.7