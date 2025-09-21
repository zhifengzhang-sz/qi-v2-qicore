# ✅ Successful MCP Installation Guide

## Final Working Configuration

This document captures the successful installation process that resulted in all 5 MCP servers working correctly.

## Key Learnings

### 1. **Use Claude Code Native Commands, Not Smithery**
- **Smithery CLI** and **Claude Code** have compatibility issues
- **Smithery** shows servers as installed but Claude Code doesn't see them
- **Claude Code native commands** work reliably

### 2. **PATH Issues with UV/UVX**
- Installing UV adds it to `~/.zshrc` but doesn't automatically update current session
- **Must run `source ~/.zshrc`** after UV installation
- UV/UVX path issues cause Qdrant server connection failures

### 3. **MCP Server Scope Configuration**
- Use `--scope user` for global access across all projects
- Project-specific installations don't inherit to other directories
- User scope stores config in `~/.claude/settings.json`

## Final Working Commands

### 1. Install Core Tools
```bash
# Install UV/UVX for Qdrant
curl -LsSf https://astral.sh/uv/install.sh | sh

# CRITICAL: Source shell configuration
source ~/.zshrc

# Verify UV is in PATH
uv --version
uvx --version

# Install Smithery (optional, for reference only)
npm install -g @smithery/cli
```

### 2. Install All MCP Servers (Working Method)
```bash
# Use Claude Code native commands with --scope user

# Sequential Thinking
claude mcp add-json --scope user "sequential-thinking" '{"command":"npx","args":["-y","@modelcontextprotocol/server-sequential-thinking"]}'

# Memory Server (PostgreSQL)
claude mcp add-json --scope user "memory" '{"command":"npx","args":["-y","mcp-memory-server"],"env":{"DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'

# Qdrant Vector Database
claude mcp add-json --scope user "qdrant" '{"command":"uvx","args":["mcp-server-qdrant"],"env":{"QDRANT_URL":"http://localhost:6333","COLLECTION_NAME":"mcp-memories","EMBEDDING_MODEL":"sentence-transformers/all-MiniLM-L6-v2"}}'

# Brave Search
claude mcp add-json --scope user "brave-search" '{"command":"npx","args":["-y","@brave/brave-search-mcp-server"],"env":{"BRAVE_API_KEY":"your-api-key-here"}}'

# Context7
claude mcp add-json --scope user "context7" '{"command":"npx","args":["-y","@upstash/context7-mcp@latest"]}'
```

### 3. Verify Installation
```bash
# From any directory (should show all 5 servers connected)
claude mcp list
```

## Expected Output
```
Checking MCP server health...

qdrant: uvx mcp-server-qdrant - ✓ Connected
memory: npx -y mcp-memory-server - ✓ Connected
brave-search: npx -y @brave/brave-search-mcp-server - ✓ Connected
context7: npx -y @upstash/context7-mcp@latest - ✓ Connected
sequential-thinking: npx -y @modelcontextprotocol/server-sequential-thinking - ✓ Connected
```

## Environment Setup Required

### ZSH Configuration (~/.zshrc)
```bash
# UV/UVX PATH (added by UV installer)
export PATH="$HOME/.cargo/bin:$PATH"

# Database URLs
export DATABASE_URL="postgresql://postgres:password@localhost:5432/cryptodb"
export QDRANT_URL="http://localhost:6333"

# API Keys
export BRAVE_API_KEY="your-brave-api-key-here"

# MCP Configuration
export QDRANT_AUTO_CREATE_COLLECTION=true
export COLLECTION_NAME="mcp-memories"
export EMBEDDING_MODEL="sentence-transformers/all-MiniLM-L6-v2"
```

### Docker Services Required
```bash
# PostgreSQL/TimescaleDB (for memory server)
docker ps | grep timescale
# Should show: qicore-timescaledb (healthy)

# Qdrant (for vector database)
docker ps | grep qdrant
# Should show: qicore-qdrant (healthy)
```

## Configuration Files Location

### User Settings (~/.claude/settings.json)
```json
{
  "model": "sonnet",
  "mcpServers": {
    "qdrant": {
      "command": "uvx",
      "args": ["mcp-server-qdrant"],
      "env": {
        "QDRANT_URL": "http://localhost:6333",
        "COLLECTION_NAME": "mcp-memories",
        "EMBEDDING_MODEL": "sentence-transformers/all-MiniLM-L6-v2"
      }
    },
    "memory": {
      "command": "npx",
      "args": ["-y", "mcp-memory-server"],
      "env": {
        "DATABASE_URL": "postgresql://postgres:password@localhost:5432/cryptodb"
      }
    },
    "brave-search": {
      "command": "npx",
      "args": ["-y", "@brave/brave-search-mcp-server"],
      "env": {
        "BRAVE_API_KEY": "your-api-key-here"
      }
    },
    "context7": {
      "command": "npx",
      "args": ["-y", "@upstash/context7-mcp@latest"]
    },
    "sequential-thinking": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-sequential-thinking"]
    }
  }
}
```

## Common Issues and Solutions

### Issue: "uvx: command not found"
**Solution**: Run `source ~/.zshrc` or restart terminal

### Issue: "No MCP servers configured" in project
**Solution**: Use `--scope user` instead of default scope

### Issue: Smithery shows installed but Claude Code doesn't see servers
**Solution**: Remove Smithery installation and use Claude Code native commands

### Issue: Qdrant fails to connect
**Solutions**:
1. Check `source ~/.zshrc` was run
2. Verify Docker container: `docker restart qicore-qdrant`
3. Test connectivity: `curl http://localhost:6333/health`

### Issue: Database connection failures
**Solutions**:
1. Verify PostgreSQL: `docker ps | grep timescale`
2. Test connection: `psql "postgresql://postgres:password@localhost:5432/cryptodb" -c "SELECT 1;"`

## Verification Commands

### System Check
```bash
# Tools
which uv uvx npm node python3

# Environment
echo $DATABASE_URL
echo $QDRANT_URL
echo $BRAVE_API_KEY

# Services
docker ps --filter "name=qicore"
curl -s http://localhost:6333/health
psql "$DATABASE_URL" -c "SELECT 1;" 2>/dev/null
```

### MCP Check
```bash
# From any directory
claude mcp list

# Test individual servers
claude mcp get qdrant
claude mcp get memory
```

## Success Criteria

✅ All tools installed (`uv`, `uvx`, `npm`, `node`)
✅ Environment variables set
✅ Docker services running (PostgreSQL, Qdrant)
✅ PATH includes `~/.cargo/bin`
✅ 5/5 MCP servers show "✓ Connected"
✅ MCP servers work from any project directory

## Next Steps

1. **Get Brave Search API Key**: Visit https://api.search.brave.com/app/keys
2. **Test MCP functionality**: Use Claude Code with the new MCP servers
3. **Set up project-specific configurations**: If needed for team sharing

This configuration has been tested and verified to work correctly across different project directories.