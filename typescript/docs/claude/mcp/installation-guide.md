# MCP Tools Installation Guide

## Overview

This guide provides detailed installation instructions for Model Context Protocol (MCP) servers used with Claude Code. MCP is an open standard that connects AI assistants to external data sources and tools.

## Prerequisites

- Node.js 18+
- Python 3.8+ (for some servers)
- Docker (for database services)
- Claude Code CLI installed

## Required Tools Installation

### 1. UV/UVX (Python Package Manager)

UV is an extremely fast Python package and project manager required for Qdrant MCP server.

```bash
# Install uv/uvx
curl -LsSf https://astral.sh/uv/install.sh | sh

# CRITICAL: Reload shell to get UV in PATH
# For ZSH users:
source ~/.zshrc
# For Bash users:
source ~/.bashrc
# or restart terminal completely

# Verify installation (should show version numbers)
uv --version
uvx --version

# IMPORTANT: You must source ~/.zshrc in EVERY new terminal session
# OR restart your terminal completely for PATH changes to take effect
```

### 2. Smithery CLI (MCP Server Manager)

Smithery provides easy installation and management of MCP servers.

```bash
# Install globally
npm install -g @smithery/cli

# Verify installation
npx @smithery/cli --version
```

## MCP Servers Installation

### 1. Sequential Thinking

Enables step-by-step reasoning and analysis for complex problem-solving.

#### Recommended Method: Claude Code Native Commands
```bash
# Add to user scope (works across all projects)
claude mcp add-json --scope user "sequential-thinking" '{"command":"npx","args":["-y","@modelcontextprotocol/server-sequential-thinking"]}'
```

#### Alternative Method: Smithery (Has Compatibility Issues)
```bash
# Note: Smithery may not integrate properly with Claude Code
# Use native Claude Code commands instead
npx @smithery/cli install sequential-thinking --client claude
```

### 2. Memory Server

Provides persistent memory capabilities using vector databases.

#### Option A: Official Memory Server (with Qdrant)
```bash
# Install Python dependencies
pip install chromadb sentence-transformers

# Test installation
npx -y @modelcontextprotocol/server-memory --help

# Add to Claude Code
claude mcp add-json "memory" '{
  "command": "npx",
  "args": ["-y", "mcp-memory-server"],
  "env": {
    "DATABASE_URL": "postgresql://postgres:password@localhost:5432/cryptodb"
  }
}'
```

#### Option B: Enhanced Memory Service
```bash
# Using Smithery
npx @smithery/cli install @doobidoo/mcp-memory-service --client claude
```

### 3. Qdrant Vector Database MCP

Connects to Qdrant vector database for semantic search and storage.

```bash
# Requires uv/uvx (install first if not done)
# Test Qdrant server is running
curl http://localhost:6333/health

# Add to Claude Code
claude mcp add-json "qdrant" '{
  "command": "uvx",
  "args": ["mcp-server-qdrant"],
  "env": {
    "QDRANT_URL": "http://localhost:6333",
    "COLLECTION_NAME": "mcp-memories",
    "EMBEDDING_MODEL": "sentence-transformers/all-MiniLM-L6-v2"
  }
}'
```

### 4. Brave Search

Web search capabilities using Brave Search API.

```bash
# Get API key from https://api.search.brave.com/app/keys
# Test installation
npx -y @brave/brave-search-mcp-server --help

# Add to Claude Code
claude mcp add-json "brave-search" '{
  "command": "npx",
  "args": ["-y", "@brave/brave-search-mcp-server"],
  "env": {
    "BRAVE_API_KEY": "YOUR_BRAVE_API_KEY_HERE"
  }
}'
```

### 5. Context7

Provides live, version-specific code documentation and examples.

```bash
# Test installation
npx -y @upstash/context7-mcp@latest --help

# Add to Claude Code
claude mcp add-json "context7" '{
  "command": "npx",
  "args": ["-y", "@upstash/context7-mcp@latest"]
}'
```

## Database Services Setup

### PostgreSQL/TimescaleDB (for Memory)

```bash
# Already running according to your setup
docker ps | grep timescale

# Connection string should be:
# postgresql://postgres:password@localhost:5432/cryptodb
```

### Qdrant Vector Database

```bash
# Check if running
docker ps | grep qdrant

# Should be accessible at:
# http://localhost:6333
```

## Configuration Management

### Using Claude Code Commands

```bash
# List current MCP servers
claude mcp list

# View server details
claude mcp view <server-name>

# Remove a server
claude mcp remove <server-name>

# Test server connection
claude mcp test <server-name>
```

### Using Smithery CLI

```bash
# List installed servers
npx @smithery/cli list --client claude

# Search available servers
npx @smithery/cli search "memory"

# Install server
npx @smithery/cli install <server-name> --client claude

# Uninstall server
npx @smithery/cli uninstall <server-name> --client claude
```

## Verification Steps

### 1. Check Tool Installations
```bash
# Verify core tools
node --version  # Should be 18+
python3 --version  # Should be 3.8+
uv --version  # Should show version
npx @smithery/cli --version  # Should show version
```

### 2. Test Database Connections
```bash
# Test PostgreSQL
psql postgresql://postgres:password@localhost:5432/cryptodb -c "SELECT 1;"

# Test Qdrant
curl http://localhost:6333/health
```

### 3. Verify MCP Servers
```bash
# List active servers in Claude Code
claude mcp list

# Test each server individually
claude mcp test sequential-thinking
claude mcp test memory
claude mcp test qdrant
claude mcp test brave-search
claude mcp test context7
```

## Troubleshooting

### Common Issues

#### 1. NPX Package Installation Timeouts
```bash
# Increase timeout
npm config set timeout 300000

# Clear npm cache
npm cache clean --force

# Use specific npm registry
npm config set registry https://registry.npmjs.org/
```

#### 2. Python Dependencies (for Memory servers)
```bash
# Install PyTorch manually first
pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cpu

# Then install other dependencies
pip install chromadb sentence-transformers
```

#### 3. UV/UVX Path Issues
```bash
# Add to PATH manually
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

#### 4. Docker Services Not Accessible
```bash
# Check Docker services
docker ps -a

# Restart services if needed
docker restart qicore-timescaledb
docker restart qicore-qdrant

# Check port bindings
netstat -tulpn | grep -E "(5432|6333)"
```

#### 5. Environment Variables
```bash
# Check if environment variables are set
env | grep -E "(DATABASE_URL|QDRANT_URL|BRAVE_API_KEY)"

# Set missing variables
export DATABASE_URL="postgresql://postgres:password@localhost:5432/cryptodb"
export QDRANT_URL="http://localhost:6333"
export BRAVE_API_KEY="your-api-key-here"
```

### Server-Specific Troubleshooting

#### Sequential Thinking
- Issue: NPX timeout during installation
- Solution: Use local package installation or Smithery

#### Memory Server
- Issue: Database connection errors
- Solution: Verify PostgreSQL is running and connection string is correct

#### Qdrant
- Issue: UV/UVX command not found
- Solution: Install UV properly and ensure it's in PATH

#### Brave Search
- Issue: API key errors
- Solution: Verify API key is valid and has proper permissions

#### Context7
- Issue: Package version conflicts
- Solution: Use latest version with @latest tag

## Best Practices

1. **Use Smithery CLI** for easier management when possible
2. **Test each server individually** before using in production
3. **Keep environment variables** in a secure configuration
4. **Monitor resource usage** as some servers can be memory-intensive
5. **Regular updates** - check for server updates periodically

## Configuration Files

### Example .claude/settings.json MCP Configuration
```json
{
  "mcpServers": {
    "sequential-thinking": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-sequential-thinking"]
    },
    "memory": {
      "command": "npx",
      "args": ["-y", "mcp-memory-server"],
      "env": {
        "DATABASE_URL": "postgresql://postgres:password@localhost:5432/cryptodb"
      }
    },
    "qdrant": {
      "command": "uvx",
      "args": ["mcp-server-qdrant"],
      "env": {
        "QDRANT_URL": "http://localhost:6333",
        "COLLECTION_NAME": "mcp-memories",
        "EMBEDDING_MODEL": "sentence-transformers/all-MiniLM-L6-v2"
      }
    },
    "brave-search": {
      "command": "npx",
      "args": ["-y", "@brave/brave-search-mcp-server"],
      "env": {
        "BRAVE_API_KEY": "YOUR_BRAVE_API_KEY_HERE"
      }
    },
    "context7": {
      "command": "npx",
      "args": ["-y", "@upstash/context7-mcp@latest"]
    }
  }
}
```

## Support and Resources

- [MCP Official Documentation](https://modelcontextprotocol.io/)
- [Claude Code MCP Guide](https://docs.claude.com/en/docs/claude-code/mcp)
- [Smithery Registry](https://smithery.ai/)
- [MCP Server Repository](https://github.com/modelcontextprotocol/servers)