# MCP Troubleshooting: Common Errors & Solutions

## Installation Errors

### Error: `uv: command not found`
```bash
# Symptoms
uvx mcp-server-qdrant
# bash: uvx: command not found

# Solution
curl -LsSf https://astral.sh/uv/install.sh | sh
source ~/.bashrc
# Or restart terminal
```

### Error: NPX Package Installation Timeout
```bash
# Symptoms
npx -y @modelcontextprotocol/server-sequential-thinking --help
# Hangs indefinitely or npm ERR! network timeout

# Immediate fix: Cancel and use global installation
# Press Ctrl+C, then:
npm install -g @modelcontextprotocol/server-sequential-thinking
npx @modelcontextprotocol/server-sequential-thinking --help

# Alternative solutions:
npm config set timeout 300000
npm cache clean --force
npm config set registry https://registry.npmjs.org/

# Use different registry if corporate firewall issues:
npm --registry https://registry.npmmirror.com install -g @modelcontextprotocol/server-sequential-thinking

# Pre-install all MCP packages globally (recommended):
npm install -g @modelcontextprotocol/server-sequential-thinking @brave/brave-search-mcp-server @upstash/context7-mcp mcp-memory-server
```

### Error: Python Dependency Conflicts
```bash
# Symptoms
pip install chromadb
# ERROR: Could not find a version that satisfies the requirement torch

# Solution
pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cpu
pip install chromadb sentence-transformers
```

## Database Connection Errors

### Error: PostgreSQL Connection Refused
```bash
# Symptoms
psql: could not connect to server: Connection refused

# Check & Fix
docker ps | grep timescale
docker restart qicore-timescaledb
docker logs qicore-timescaledb

# Verify port
netstat -tulpn | grep 5432
```

### Error: Qdrant Not Accessible
```bash
# Symptoms
curl http://localhost:6333/health
# curl: (7) Failed to connect to localhost port 6333

# Check & Fix
docker ps | grep qdrant
docker restart qicore-qdrant
docker logs qicore-qdrant

# Check health status
docker ps --format "table {{.Names}}\t{{.Status}}"
```

## MCP Server Runtime Errors

### Error: `BRAVE_API_KEY` Not Set
```bash
# Symptoms
Error: Missing required environment variable: BRAVE_API_KEY

# Solution
export BRAVE_API_KEY="your-api-key-here"
# Add to ~/.bashrc for persistence
echo 'export BRAVE_API_KEY="your-api-key-here"' >> ~/.bashrc
```

### Error: Memory Server Database Schema
```bash
# Symptoms
ERROR: relation "memories" does not exist

# Solution
psql $DATABASE_URL -c "
CREATE TABLE IF NOT EXISTS memories (
    id SERIAL PRIMARY KEY,
    content TEXT NOT NULL,
    embedding VECTOR(384),
    created_at TIMESTAMP DEFAULT NOW()
);"
```

### Error: Qdrant Collection Not Found
```bash
# Symptoms
CollectionNotFoundError: Collection 'mcp-memories' not found

# Solution - Auto-create collection
export QDRANT_AUTO_CREATE_COLLECTION=true
# Or create manually via Qdrant API
curl -X PUT "http://localhost:6333/collections/mcp-memories" \
-H "Content-Type: application/json" \
-d '{
    "vectors": {
        "size": 384,
        "distance": "Cosine"
    }
}'
```

## Claude Code MCP Integration Errors

### Error: MCP Server Won't Start
```bash
# Symptoms
claude mcp list
# Server 'sequential-thinking': Failed to start

# Debug
claude mcp test sequential-thinking --debug
# Check logs in ~/.claude/logs/

# Common fixes
npm cache clean --force
npx -y @modelcontextprotocol/server-sequential-thinking --version
```

### Error: Permission Denied
```bash
# Symptoms
Error: EACCES: permission denied, open '/home/user/.npm/_logs/debug.log'

# Solution
sudo chown -R $(whoami) ~/.npm
sudo chown -R $(whoami) ~/.cache
```

### Error: Port Already in Use
```bash
# Symptoms
Error: listen EADDRINUSE: address already in use :::8000

# Find & kill process
lsof -i :8000
kill -9 <PID>

# Or use different port
export MCP_SERVER_PORT=8001
```

## Performance Issues

### Error: High Memory Usage
```bash
# Symptoms
System becomes slow, memory usage high

# Solutions
# Limit embedding model size
export EMBEDDING_MODEL="sentence-transformers/all-MiniLM-L6-v2"  # Smaller model

# Monitor memory
docker stats
htop
```

### Error: Slow Response Times
```bash
# Symptoms
MCP calls take very long to respond

# Solutions
# Check database indices
psql $DATABASE_URL -c "CREATE INDEX IF NOT EXISTS idx_memories_embedding ON memories USING ivfflat (embedding vector_cosine_ops);"

# Verify Qdrant performance
curl "http://localhost:6333/metrics"
```

## Network & Connectivity Issues

### Error: Docker Networks
```bash
# Symptoms
Cannot connect to database from MCP server

# Solution
# Check Docker network
docker network ls
docker network inspect bridge

# Ensure containers are on same network
docker-compose ps
```

### Error: WSL2 Port Forwarding
```bash
# Symptoms (WSL2 specific)
Cannot access localhost:6333 from Windows

# Solution
# Forward ports in WSL2
netsh interface portproxy add v4tov4 listenport=6333 listenaddress=0.0.0.0 connectport=6333 connectaddress=$(wsl hostname -I)
```

## Configuration Errors

### Error: Invalid JSON Configuration
```bash
# Symptoms
Error parsing MCP server configuration

# Solution
# Validate JSON
cat ~/.claude/settings.json | jq .
# Fix syntax errors

# Use proper escaping
claude mcp add-json "server" '{"command":"npx","args":["-y","package"]}'
```

### Error: Environment Variables Not Loaded
```bash
# Symptoms
MCP server starts but can't access databases

# Solution
# Check variables are set
env | grep -E "(DATABASE_URL|QDRANT_URL|BRAVE_API_KEY)"

# Restart Claude Code after setting variables
claude exit
export DATABASE_URL="postgresql://postgres:password@localhost:5432/cryptodb"
claude
```

## Debugging Commands

### Full System Check
```bash
#!/bin/bash
echo "=== System Check ==="
echo "Node: $(node --version)"
echo "NPM: $(npm --version)"
echo "Python: $(python3 --version)"
echo "UV: $(uv --version 2>/dev/null || echo 'Not installed')"

echo -e "\n=== Database Check ==="
docker ps --filter "name=qicore" --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"

echo -e "\n=== Database Health ==="
curl -s http://localhost:6333/health && echo " ✓ Qdrant OK" || echo " ✗ Qdrant Failed"
psql $DATABASE_URL -c "SELECT 1;" 2>/dev/null && echo "✓ PostgreSQL OK" || echo "✗ PostgreSQL Failed"

echo -e "\n=== MCP Servers ==="
claude mcp list

echo -e "\n=== Environment ==="
echo "DATABASE_URL: ${DATABASE_URL:0:20}..."
echo "QDRANT_URL: $QDRANT_URL"
echo "BRAVE_API_KEY: ${BRAVE_API_KEY:0:10}..."
```

### Individual Server Testing
```bash
# Test each server individually
for server in sequential-thinking memory qdrant brave-search context7; do
    echo "Testing $server..."
    claude mcp test $server && echo "✓ $server OK" || echo "✗ $server Failed"
done
```

### Log Analysis
```bash
# Check Claude Code logs
tail -f ~/.claude/logs/mcp.log

# Check Docker logs
docker logs qicore-timescaledb --tail 50
docker logs qicore-qdrant --tail 50

# Check system logs
journalctl -u docker --tail 50
```

## Recovery Procedures

### Complete Reset
```bash
# Remove all MCP servers
claude mcp list | grep -v "No MCP servers" | xargs -I {} claude mcp remove {}

# Clear caches
npm cache clean --force
pip cache purge

# Restart services
docker restart qicore-timescaledb qicore-qdrant

# Reinstall everything
# Follow installation guide from scratch
```

### Selective Reset
```bash
# Reset specific server
claude mcp remove problematic-server
npm cache clean --force
claude mcp add-json "problematic-server" '{"command":"npx","args":["-y","package"]}'
```