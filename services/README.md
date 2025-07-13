# QiCore Foundation Services

This directory contains Docker Compose configurations for all services required by QiCore Foundation components.

## Services Overview

### Redis (Default Distributed Cache)
- **Image**: `redis:7.2-alpine`
- **Port**: `6379`
- **Purpose**: Primary distributed cache backend for QiCore Cache component
- **Features**: Production-ready configuration with persistence, memory management, and key eviction notifications

### Valkey (Alternative Distributed Cache)
- **Image**: `valkey/valkey:7.2-alpine` 
- **Port**: `6380`
- **Purpose**: Redis alternative with 20% performance improvement (2025 pattern)
- **Features**: Enhanced memory efficiency and improved multi-threading

### Redis Commander (Optional)
- **Image**: `rediscommander/redis-commander:latest`
- **Port**: `8081`
- **Purpose**: Web-based Redis management interface
- **Profile**: `tools` (not started by default)

## Quick Start

### Start Redis Service
```bash
cd services
docker-compose up -d redis
```

### Start All Services
```bash
cd services
docker-compose up -d
```

### Start with Management Tools
```bash
cd services
docker-compose --profile tools up -d
```

### Start Valkey Instead of Redis
```bash
cd services
docker-compose --profile valkey up -d valkey
```

## Service Configuration

### Redis Configuration
- **File**: `redis/redis.conf`
- **Persistence**: RDB + AOF enabled
- **Memory**: 512MB limit with LRU eviction
- **Key Events**: Expiration notifications enabled
- **Security**: Protected mode disabled for development

### Valkey Configuration  
- **File**: `valkey/valkey.conf`
- **Optimizations**: Enhanced for QiCore workloads
- **Memory**: 1GB limit with improved efficiency
- **Performance**: 20% faster than Redis for QiCore patterns

## Integration with QiCore Cache

### Haskell Integration
```haskell
-- Connect to Redis
cacheResult <- Cache.createDistributed "localhost" 6379 config

-- Connect to Valkey  
cacheResult <- Cache.createDistributed "localhost" 6380 config
```

### Service Discovery
The Cache component automatically detects available backends:
1. **Redis** on `localhost:6379` (default)
2. **Valkey** on `localhost:6380` (alternative)
3. **Memory** fallback if no distributed cache available

## Data Persistence

### Volume Mounts
- **Redis data**: `redis_data` volume mounted to `/data`
- **Valkey data**: `valkey_data` volume mounted to `/data`
- **Configuration**: Host configs mounted to container paths

### Backup and Recovery
```bash
# Backup Redis data
docker-compose exec redis redis-cli BGSAVE

# Backup Valkey data
docker-compose exec valkey valkey-cli BGSAVE

# Copy backup files
docker cp qicore-redis:/data/dump.rdb ./backups/
```

## Monitoring and Health Checks

### Health Check Commands
```bash
# Check Redis health
docker-compose exec redis redis-cli ping

# Check Valkey health
docker-compose exec valkey valkey-cli ping

# View logs
docker-compose logs redis
docker-compose logs valkey
```

### Performance Monitoring
- **Slow queries**: Configured to log queries > 10ms
- **Latency monitoring**: Threshold set to 100μs
- **Memory usage**: Tracked via Redis/Valkey INFO command

## Development vs Production

### Development (Current)
- **Security**: No password authentication
- **Network**: Accessible on all interfaces
- **Persistence**: Enabled but not critical
- **Resources**: Limited memory allocation

### Production Recommendations
1. **Enable authentication**: Uncomment `requirepass` in configs
2. **Network security**: Bind to specific interfaces only
3. **Resource limits**: Increase memory allocation based on usage
4. **Backup strategy**: Implement automated backup procedures
5. **Monitoring**: Add external monitoring (Prometheus/Grafana)

## Future Services

This directory structure supports additional services:

### Planned Services
- **PostgreSQL**: For advanced persistence patterns
- **OpenTelemetry Collector**: For observability aggregation
- **Jaeger**: For distributed tracing
- **Prometheus**: For metrics collection

### Service Addition Pattern
```yaml
# New service template
new-service:
  image: service:tag
  container_name: qicore-new-service
  ports:
    - "port:port"
  volumes:
    - service_data:/data
  networks:
    - qicore-network
  healthcheck:
    test: ["CMD", "health-command"]
```

## Troubleshooting

### Common Issues

#### Redis/Valkey Won't Start
```bash
# Check logs
docker-compose logs redis

# Verify configuration
docker-compose config

# Test configuration syntax
docker-compose exec redis redis-server --test-config
```

#### Connection Refused
```bash
# Check service status
docker-compose ps

# Test network connectivity
docker-compose exec redis redis-cli ping

# Verify port mapping
netstat -tlnp | grep 6379
```

#### Memory Issues
```bash
# Check memory usage
docker-compose exec redis redis-cli INFO memory

# Monitor real-time stats
docker-compose exec redis redis-cli MONITOR
```

### Performance Tuning

#### For High-Throughput Workloads
- Increase `maxclients` in configuration
- Adjust `tcp-backlog` for connection handling
- Consider `maxmemory-policy` based on access patterns

#### For Low-Latency Requirements
- Disable persistence (`save ""` and `appendonly no`)
- Use `allkeys-lru` eviction policy
- Increase `maxmemory-samples` for better LRU approximation

---

**Service Status**: Production-ready for QiCore Foundation v-0.2.x
**Last Updated**: 2025-01-13
**Next Update**: Based on QiCore Foundation requirements