# ZSH Setup for MCP Tools

## ZSH Configuration (.zshrc)

Since you're using ZSH, here are the specific commands for your shell:

### 1. Add UV to PATH
```bash
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> ~/.zshrc
```

### 2. Add Environment Variables
```bash
# Database configuration
echo 'export DATABASE_URL="postgresql://postgres:password@localhost:5432/cryptodb"' >> ~/.zshrc

# Qdrant configuration
echo 'export QDRANT_URL="http://localhost:6333"' >> ~/.zshrc

# Brave Search API key (replace with your actual key)
echo 'export BRAVE_API_KEY="your-brave-api-key-here"' >> ~/.zshrc

# Additional MCP configuration
echo 'export QDRANT_AUTO_CREATE_COLLECTION=true' >> ~/.zshrc
echo 'export COLLECTION_NAME="mcp-memories"' >> ~/.zshrc
echo 'export EMBEDDING_MODEL="sentence-transformers/all-MiniLM-L6-v2"' >> ~/.zshrc
```

### 3. Reload Configuration
```bash
source ~/.zshrc
```

### 4. Verify Setup
```bash
echo "PATH includes UV: $(echo $PATH | grep -o '.cargo/bin' || echo 'NOT FOUND')"
echo "DATABASE_URL: ${DATABASE_URL:0:30}..."
echo "QDRANT_URL: $QDRANT_URL"
echo "BRAVE_API_KEY: ${BRAVE_API_KEY:0:10}..."
```

## Alternative: One-Line Setup

```bash
# Add all at once
cat >> ~/.zshrc << 'EOF'

# MCP Tools Configuration
export PATH="$HOME/.cargo/bin:$PATH"
export DATABASE_URL="postgresql://postgres:password@localhost:5432/cryptodb"
export QDRANT_URL="http://localhost:6333"
export BRAVE_API_KEY="your-brave-api-key-here"
export QDRANT_AUTO_CREATE_COLLECTION=true
export COLLECTION_NAME="mcp-memories"
export EMBEDDING_MODEL="sentence-transformers/all-MiniLM-L6-v2"
EOF

# Reload
source ~/.zshrc
```

## Claude MCP Commands (Corrected)

The correct syntax for Claude MCP commands:

### 1. List MCP Servers
```bash
claude mcp list
```

### 2. Add Server with JSON (Correct Syntax)
```bash
# Basic syntax
claude mcp add-json <name> '<json-config>'

# With scope options
claude mcp add-json --scope local <name> '<json-config>'
claude mcp add-json --scope user <name> '<json-config>'
claude mcp add-json --scope project <name> '<json-config>'
```

### 3. Correct Examples

#### Sequential Thinking
```bash
claude mcp add-json "sequential-thinking" '{"command":"npx","args":["-y","@modelcontextprotocol/server-sequential-thinking"]}'
```

#### Memory Server
```bash
claude mcp add-json "memory" '{"command":"npx","args":["-y","mcp-memory-server"],"env":{"DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'
```

#### Qdrant
```bash
claude mcp add-json "qdrant" '{"command":"uvx","args":["mcp-server-qdrant"],"env":{"QDRANT_URL":"http://localhost:6333","COLLECTION_NAME":"mcp-memories","EMBEDDING_MODEL":"sentence-transformers/all-MiniLM-L6-v2"}}'
```

#### Brave Search
```bash
claude mcp add-json "brave-search" '{"command":"npx","args":["-y","@brave/brave-search-mcp-server"],"env":{"BRAVE_API_KEY":"your-api-key-here"}}'
```

#### Context7
```bash
claude mcp add-json "context7" '{"command":"npx","args":["-y","@upstash/context7-mcp@latest"]}'
```

### 4. Other MCP Commands
```bash
# Remove server
claude mcp remove <name>

# Get server details
claude mcp get <name>

# Simple add (for basic configurations)
claude mcp add <name> <command> [args...]
```

## Configuration Scopes

- **local**: Project-specific (stored in project directory)
- **user**: User-wide (stored in ~/.claude/)
- **project**: Shared project config (committed to git)

## Troubleshooting ZSH Issues

### Environment Variables Not Loading
```bash
# Check if variables are set
env | grep -E "(DATABASE_URL|QDRANT_URL|BRAVE_API_KEY)"

# If not set, reload zsh
source ~/.zshrc

# Or restart terminal
exec zsh
```

### PATH Issues
```bash
# Check if UV is in PATH
which uv uvx

# If not found, manually add to current session
export PATH="$HOME/.cargo/bin:$PATH"

# Check .zshrc syntax
cat ~/.zshrc | tail -20
```

### JSON Escaping in ZSH
```bash
# Use single quotes to avoid escaping issues
claude mcp add-json "server" '{"key":"value"}'

# If you need double quotes inside JSON, escape them
claude mcp add-json "server" "{\"key\":\"value\"}"
```

## Verification Script for ZSH

```bash
#!/bin/zsh
echo "=== ZSH MCP Setup Verification ==="

echo "\n1. Shell Detection:"
echo "Current shell: $0"
echo "ZSH version: $ZSH_VERSION"

echo "\n2. Tools in PATH:"
which uv && echo "✓ UV found" || echo "✗ UV not found"
which uvx && echo "✓ UVX found" || echo "✗ UVX not found"
which npx && echo "✓ NPX found" || echo "✗ NPX not found"

echo "\n3. Environment Variables:"
[[ -n "$DATABASE_URL" ]] && echo "✓ DATABASE_URL set" || echo "✗ DATABASE_URL not set"
[[ -n "$QDRANT_URL" ]] && echo "✓ QDRANT_URL set" || echo "✗ QDRANT_URL not set"
[[ -n "$BRAVE_API_KEY" ]] && echo "✓ BRAVE_API_KEY set" || echo "✗ BRAVE_API_KEY not set"

echo "\n4. Database Connectivity:"
curl -s "$QDRANT_URL/health" >/dev/null && echo "✓ Qdrant accessible" || echo "✗ Qdrant not accessible"
psql "$DATABASE_URL" -c "SELECT 1;" 2>/dev/null >/dev/null && echo "✓ PostgreSQL accessible" || echo "✗ PostgreSQL not accessible"

echo "\n5. MCP Servers:"
claude mcp list 2>/dev/null || echo "No MCP servers configured"
```

Save this as `check-mcp-setup.sh`, make it executable (`chmod +x check-mcp-setup.sh`), and run it to verify your setup.