# MCP Troubleshooting Guide

## Quick Diagnostics

```bash
# Check all systems
echo "=== System Check ==="
which uv uvx npm node python3
echo -e "\n=== Environment ==="
echo "DATABASE_URL: ${DATABASE_URL:0:30}..."
echo "QDRANT_URL: $QDRANT_URL"
echo "CHROMA_PATH: $CHROMA_PATH"
echo -e "\n=== Services ==="
docker ps --filter "name=qicore"
curl -s http://localhost:6333/health && echo "✓ Qdrant OK" || echo "✗ Qdrant Failed"
psql "$DATABASE_URL" -c "SELECT 1;" 2>/dev/null && echo "✓ PostgreSQL OK" || echo "✗ PostgreSQL Failed"
echo -e "\n=== MCP Servers ==="
claude mcp list
```

## Common Issues

### 1. `uv: command not found`

**Problem**: Qdrant MCP server fails with "uvx: command not found"

**Solution**:
```bash
# Install UV
curl -LsSf https://astral.sh/uv/install.sh | sh

# CRITICAL: Reload shell
source ~/.zshrc

# Verify
uv --version && uvx --version
```

### 2. NPX Installation Timeouts

**Problem**: `npx -y @modelcontextprotocol/server-sequential-thinking` hangs

**Solutions**:
```bash
# Method 1: Increase timeout and clear cache
npm config set timeout 300000
npm cache clean --force

# Method 2: Pre-install packages
npm install -g @modelcontextprotocol/server-sequential-thinking @brave/brave-search-mcp-server

# Method 3: Use different registry
npm --registry https://registry.npmmirror.com install -g package-name
```

### 3. Database Connection Failures

**Problem**: "could not connect to server: Connection refused"

**Solutions**:
```bash
# Check containers
docker ps | grep -E "(timescale|qdrant)"

# Restart services
docker restart qicore-timescaledb qicore-qdrant

# Check ports
netstat -tulpn | grep -E "(5432|6333)"

# Test connections
psql "$DATABASE_URL" -c "SELECT version();"
curl http://localhost:6333/health
```

### 4. MCP Servers Not Detected

**Problem**: Claude shows "No MCP servers configured" despite installation

**Solutions**:
```bash
# Check configuration location
cat ~/.claude/settings.json

# Verify correct scope was used
claude mcp list  # Should show servers

# If not working, reinstall with explicit scope
claude mcp remove server-name
claude mcp add-json --scope user "server-name" '{"config":"here"}'

# Restart Claude completely
pkill -f claude
claude
```

### 5. ChromaDB Permission Errors

**Problem**: "Permission denied" when accessing ~/.chromadb/

**Solutions**:
```bash
# Fix ownership and permissions
sudo chown -R $(whoami):$(whoami) ~/.chromadb/
chmod -R 755 ~/.chromadb/

# Recreate directories if needed
rm -rf ~/.chromadb/data/*
mkdir -p ~/.chromadb/{data,collections}
chmod 755 ~/.chromadb ~/.chromadb/data
```

### 6. GitHub/Brave Search API Errors

**Problem**: "Missing required environment variable" or "Invalid API key"

**Solutions**:
```bash
# Check environment variables
env | grep -E "(GITHUB|BRAVE)"

# Set API keys
export GITHUB_PERSONAL_ACCESS_TOKEN="your-token-here"
export BRAVE_API_KEY="your-key-here"

# Add to ~/.zshrc for persistence
echo 'export GITHUB_PERSONAL_ACCESS_TOKEN="your-token"' >> ~/.zshrc
echo 'export BRAVE_API_KEY="your-key"' >> ~/.zshrc
source ~/.zshrc

# Remove and re-add servers with correct keys
claude mcp remove github
claude mcp add-json --scope user "github" '{"command":"npx","args":["-y","@modelcontextprotocol/server-github"],"env":{"GITHUB_PERSONAL_ACCESS_TOKEN":"your-token","DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'
```

### 7. Smithery Compatibility Issues

**Problem**: Smithery shows servers installed but Claude Code doesn't see them

**Solution**: **Don't use Smithery** - it has compatibility issues with Claude Code

```bash
# Remove Smithery installations
npx @smithery/cli list --client claude
npx @smithery/cli uninstall server-name --client claude

# Use Claude Code native commands instead
claude mcp add-json --scope user "server-name" '{"command":"npx","args":["-y","package-name"]}'
```

### 8. Docker Container Issues

**Problem**: Containers not healthy or accessible

**Solutions**:
```bash
# Check container status
docker ps -a | grep qicore

# Check logs
docker logs qicore-timescaledb --tail 20
docker logs qicore-qdrant --tail 20

# Restart unhealthy containers
docker restart qicore-timescaledb qicore-qdrant

# Check port conflicts
lsof -i :5432  # PostgreSQL
lsof -i :6333  # Qdrant
lsof -i :8000  # ChromaDB
```

### 9. Path and Environment Issues

**Problem**: Commands work in home directory but not in project directory

**Solutions**:
```bash
# Check if PATH is different
echo $PATH

# Ensure environment is loaded
source ~/.zshrc

# Check current working directory
pwd

# Verify Claude Code configuration scope
cd /your/project/directory
claude mcp list
```

### 10. Memory and Performance Issues

**Problem**: System becomes slow or runs out of memory

**Solutions**:
```bash
# Monitor memory usage
docker stats
htop

# Check disk space
df -h
du -sh ~/.chromadb/ ~/.claude/

# Restart services to free memory
docker restart qicore-timescaledb qicore-qdrant
```

## Recovery Procedures

### Complete Reset

```bash
# 1. Remove all MCP servers
for server in sequential-thinking memory qdrant chromadb brave-search context7 github filesystem docker postgresql; do
    claude mcp remove $server 2>/dev/null || true
done

# 2. Clear caches
npm cache clean --force
pip cache purge 2>/dev/null || true

# 3. Restart Docker services
docker restart qicore-timescaledb qicore-qdrant qicore-redis

# 4. Reset ChromaDB
rm -rf ~/.chromadb/data/*
mkdir -p ~/.chromadb/{data,collections}
chmod 755 ~/.chromadb ~/.chromadb/data

# 5. Reinstall everything
# Follow installation commands from guide.md
```

### Selective Reset

```bash
# Reset specific problematic server
claude mcp remove problematic-server

# Clear related caches
npm cache clean --force

# Check prerequisites
which uv uvx npm node

# Reinstall server
claude mcp add-json --scope user "server-name" '{"command":"npx","args":["-y","package"]}'
```

## Debugging Commands

### MCP Server Health

```bash
# Test individual servers
claude mcp get server-name

# Debug with verbose output
claude --debug mcp list

# Check MCP logs
tail -f ~/.claude/logs/mcp.log 2>/dev/null || echo "No MCP logs found"
```

### Database Debugging

```bash
# PostgreSQL debugging
psql "$DATABASE_URL" -c "\dt mcp_*.*"  # List MCP tables
psql "$DATABASE_URL" -c "SELECT schemaname FROM pg_tables WHERE schemaname LIKE 'mcp_%';"

# ChromaDB debugging
ls -la ~/.chromadb/data/
python3 -c "import chromadb; client = chromadb.PersistentClient(path='~/.chromadb/data'); print([c.name for c in client.list_collections()])"

# Qdrant debugging
curl -X GET "http://localhost:6333/collections"
```

### Network Debugging

```bash
# Check port availability
netstat -tulpn | grep -E "(5432|6333|8000)"

# Test connectivity
telnet localhost 5432
telnet localhost 6333

# Check firewall (Ubuntu)
sudo ufw status
```

## Performance Optimization

### Storage Cleanup

```bash
# Clean up old conversation data
psql "$DATABASE_URL" -c "DELETE FROM mcp_memory.conversations WHERE created_at < NOW() - INTERVAL '30 days';"

# ChromaDB maintenance
python3 -c "
import chromadb
client = chromadb.PersistentClient(path='~/.chromadb/data')
for collection in client.list_collections():
    print(f'{collection.name}: {collection.count()} documents')
"
```

### Monitor Resource Usage

```bash
# Create monitoring script
cat > ~/.claude/health_check.sh << 'EOF'
#!/bin/bash
echo "=== Health Check $(date) ==="

echo -e "\n📊 Memory Usage:"
free -h

echo -e "\n💾 Disk Usage:"
df -h | grep -E "(/$|home)"
echo "ChromaDB: $(du -sh ~/.chromadb/ 2>/dev/null | cut -f1)"

echo -e "\n🐳 Container Status:"
docker stats --no-stream --format "table {{.Name}}\t{{.CPUPerc}}\t{{.MemUsage}}"

echo -e "\n🔗 MCP Servers:"
claude mcp list | grep -E "(✓|✗)"

echo -e "\n💡 Database Health:"
psql "$DATABASE_URL" -c "SELECT COUNT(*) as conversations FROM mcp_memory.conversations;" 2>/dev/null || echo "PostgreSQL: Error"
curl -s http://localhost:6333/health > /dev/null && echo "Qdrant: OK" || echo "Qdrant: Error"
EOF

chmod +x ~/.claude/health_check.sh
```

## Support Resources

- **Claude Code Issues**: https://github.com/anthropics/claude-code/issues
- **MCP Documentation**: https://modelcontextprotocol.io/
- **TimescaleDB Docs**: https://docs.timescale.com/
- **ChromaDB Docs**: https://docs.trychroma.com/
- **Qdrant Docs**: https://qdrant.tech/documentation/

## When to Seek Help

Create an issue with this information:

```bash
# Collect debug information
echo "=== Debug Info ==="
echo "OS: $(uname -a)"
echo "Node: $(node --version)"
echo "Python: $(python3 --version)"
echo "Docker: $(docker --version)"
echo "UV: $(uv --version 2>/dev/null || echo 'Not installed')"
echo -e "\nMCP Servers:"
claude mcp list
echo -e "\nDocker Containers:"
docker ps --filter "name=qicore"
echo -e "\nEnvironment:"
env | grep -E "(DATABASE_URL|QDRANT_URL|CHROMA_PATH)" | sed 's/=.*/=***/'
```