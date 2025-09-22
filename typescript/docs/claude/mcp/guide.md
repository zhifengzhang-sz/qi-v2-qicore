# Claude Code MCP Complete Guide

## Overview

Complete guide for setting up **10 MCP servers** with integrated **TimescaleDB + ChromaDB + Qdrant** architecture for Claude Code.

## Architecture

### High-Level Architecture

```mermaid
graph TB
    Claude[Claude Code CLI] --> MCP[MCP Layer]

    MCP --> ST[Sequential Thinking]
    MCP --> MEM[Memory Server]
    MCP --> QD[Qdrant MCP]
    MCP --> CHR[ChromaDB MCP]
    MCP --> BS[Brave Search]
    MCP --> CTX[Context7]
    MCP --> GH[GitHub MCP]
    MCP --> FS[File System MCP]
    MCP --> DK[Docker MCP]
    MCP --> PG[PostgreSQL MCP]

    ST --> CHR
    MEM --> TSC[TimescaleDB]
    MEM --> CHR
    QD --> QDB[(Qdrant Vector DB)]
    CHR --> CDB[(ChromaDB Local)]
    GH --> TSC
    GH --> CHR
    FS --> TSC
    DK --> DOCKER[Docker Engine]
    PG --> TSC

    TSC --> PGDB[(PostgreSQL)]
    DOCKER --> CONT1[qicore-timescaledb]
    DOCKER --> CONT2[qicore-qdrant]
    DOCKER --> CONT3[qicore-redis]

    classDef mcpServer fill:#e1f5fe,stroke:#01579b,stroke-width:2px
    classDef database fill:#f3e5f5,stroke:#4a148c,stroke-width:2px
    classDef container fill:#fff3e0,stroke:#e65100,stroke-width:2px

    class ST,MEM,QD,CHR,BS,CTX,GH,FS,DK,PG mcpServer
    class TSC,PGDB,QDB,CDB database
    class CONT1,CONT2,CONT3 container
```

### Database Responsibilities

| Database | Purpose | Use Cases |
|----------|---------|-----------|
| **TimescaleDB** | Structured data, time-series | User settings, operations history, GitHub repos/issues |
| **ChromaDB** | Local vector storage | Conversation memory, code snippets, semantic search |
| **Qdrant** | High-performance vectors | Large-scale embeddings, production vector ops |

## Quick Installation

### Prerequisites

```bash
# Verify requirements
node --version          # 18+
python3 --version       # 3.8+
docker --version        # Any recent version
uv --version           # Install if missing

# Check Docker services
docker ps | grep -E "(timescale|qdrant|redis|chromadb)"
```

### Environment Setup

```bash
# Add to ~/.zshrc (or ~/.bashrc for Bash users)
cat >> ~/.zshrc << 'EOF'

# Claude MCP Configuration
export PATH="$HOME/.cargo/bin:$PATH"
export DATABASE_URL="postgresql://postgres:password@localhost:5432/cryptodb"
export CHROMA_URL="http://localhost:8000"
export QDRANT_URL="http://localhost:6333"
export GITHUB_PERSONAL_ACCESS_TOKEN="your-github-token-here"
export BRAVE_API_KEY="your-brave-api-key-here"
export ALLOWED_DIRECTORIES="$HOME/dev,$HOME/projects,$HOME/.claude"

EOF

# Reload configuration
source ~/.zshrc
```

**Note**: Customize `ALLOWED_DIRECTORIES` to match your actual development directories. Common examples:
- `$HOME/dev,$HOME/projects,$HOME/workspace,$HOME/.claude`
- `$HOME/code,$HOME/repositories,$HOME/.claude`
- `/workspace,$HOME/dev,$HOME/.claude` (for Docker dev environments)

### Database Setup

```bash
# Start Docker services (if not already running)
docker-compose up -d timescaledb qdrant chromadb redis

# Wait for services to be healthy
sleep 10

# TimescaleDB schemas
psql "postgresql://postgres:password@localhost:5432/cryptodb" << 'EOF'
CREATE SCHEMA IF NOT EXISTS mcp_core;
CREATE SCHEMA IF NOT EXISTS mcp_memory;
CREATE SCHEMA IF NOT EXISTS mcp_github;
CREATE SCHEMA IF NOT EXISTS mcp_filesystem;
CREATE SCHEMA IF NOT EXISTS mcp_docker;

CREATE TABLE IF NOT EXISTS mcp_memory.conversations (
    id SERIAL PRIMARY KEY,
    session_id UUID NOT NULL,
    content TEXT NOT NULL,
    embedding_source VARCHAR(50) DEFAULT 'chromadb',
    metadata JSONB,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS mcp_github.repositories (
    id SERIAL PRIMARY KEY,
    owner VARCHAR(255) NOT NULL,
    name VARCHAR(255) NOT NULL,
    full_name VARCHAR(255) UNIQUE NOT NULL,
    last_accessed TIMESTAMPTZ DEFAULT NOW(),
    metadata JSONB
);

CREATE TABLE IF NOT EXISTS mcp_filesystem.operations (
    id SERIAL PRIMARY KEY,
    operation_type VARCHAR(50) NOT NULL,
    file_path TEXT NOT NULL,
    success BOOLEAN DEFAULT TRUE,
    executed_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS mcp_docker.container_logs (
    id SERIAL PRIMARY KEY,
    container_name VARCHAR(255) NOT NULL,
    action VARCHAR(50) NOT NULL,
    status VARCHAR(50),
    executed_at TIMESTAMPTZ DEFAULT NOW()
);

SELECT create_hypertable('mcp_memory.conversations', 'created_at', if_not_exists => TRUE);
SELECT create_hypertable('mcp_filesystem.operations', 'executed_at', if_not_exists => TRUE);
SELECT create_hypertable('mcp_docker.container_logs', 'executed_at', if_not_exists => TRUE);

GRANT ALL PRIVILEGES ON SCHEMA mcp_core TO postgres;
GRANT ALL PRIVILEGES ON SCHEMA mcp_memory TO postgres;
GRANT ALL PRIVILEGES ON SCHEMA mcp_github TO postgres;
GRANT ALL PRIVILEGES ON SCHEMA mcp_filesystem TO postgres;
GRANT ALL PRIVILEGES ON SCHEMA mcp_docker TO postgres;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA mcp_memory TO postgres;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA mcp_github TO postgres;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA mcp_filesystem TO postgres;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA mcp_docker TO postgres;
EOF

# Verify ChromaDB is accessible
curl -f http://localhost:8000/api/v1/heartbeat && echo "✓ ChromaDB ready"
```

### MCP Server Installation

**Run these commands one at a time:**

```bash
# Core AI and Memory
claude mcp add-json --scope user "sequential-thinking" '{"command":"npx","args":["-y","@modelcontextprotocol/server-sequential-thinking"]}'
claude mcp add-json --scope user "memory" '{"command":"npx","args":["-y","mcp-memory-server"],"env":{"DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'

# Vector Databases
claude mcp add-json --scope user "qdrant" '{"command":"uvx","args":["mcp-server-qdrant"],"env":{"QDRANT_URL":"http://localhost:6333","COLLECTION_NAME":"mcp-memories","EMBEDDING_MODEL":"sentence-transformers/all-MiniLM-L6-v2"}}'
claude mcp add-json --scope user "chromadb" '{"command":"npx","args":["-y","chroma-mcp-server"],"env":{"CHROMA_URL":"http://localhost:8000"}}'

# External Services
claude mcp add-json --scope user "brave-search" '{"command":"npx","args":["-y","@brave/brave-search-mcp-server"],"env":{"BRAVE_API_KEY":"your-brave-api-key-here"}}'
claude mcp add-json --scope user "context7" '{"command":"npx","args":["-y","@upstash/context7-mcp@latest"]}'

# Development Tools
claude mcp add-json --scope user "github" '{"command":"npx","args":["-y","@modelcontextprotocol/server-github"],"env":{"GITHUB_PERSONAL_ACCESS_TOKEN":"your-github-token-here","DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'
claude mcp add-json --scope user "filesystem" '{"command":"npx","args":["-y","@modelcontextprotocol/server-filesystem"],"env":{"ALLOWED_DIRECTORIES":"$HOME/dev,$HOME/projects,$HOME/.claude","DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'
claude mcp add-json --scope user "docker" '{"command":"npx","args":["-y","@modelcontextprotocol/server-docker"],"env":{"DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'

# Database Operations
claude mcp add-json --scope user "postgresql" '{"command":"npx","args":["-y","@modelcontextprotocol/server-postgres"],"env":{"DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'
```

### Verification

```bash
# Should show 10 connected servers
claude mcp list
```

**Expected output:**
```
sequential-thinking: ✓ Connected
memory: ✓ Connected
qdrant: ✓ Connected
chromadb: ✓ Connected
brave-search: ✓ Connected
context7: ✓ Connected
github: ✓ Connected
filesystem: ✓ Connected
docker: ✓ Connected
postgresql: ✓ Connected
```

## API Keys Required

### GitHub Personal Access Token
1. Visit: https://github.com/settings/personal-access-tokens
2. Scopes: `repo`, `issues`, `pull_requests`, `actions`
3. Replace `your-github-token-here` in commands above

### Brave Search API Key
1. Visit: https://api.search.brave.com/app/keys
2. Create free account and generate key
3. Replace `your-brave-api-key-here` in commands above

## Usage Examples

### Conversation Memory
```
"Remember this error pattern and solution for future reference"
```

### GitHub Integration
```
"Show me recent issues in my repositories"
"Create a pull request for this feature"
```

### File Operations
```
"Read all TypeScript files in this project"
"Find similar code patterns to this function"
```

### Container Management
```
"Check TimescaleDB container health"
"Restart the Qdrant service"
```

### Semantic Search
```
"Find conversations similar to database optimization"
"Search for code snippets related to error handling"
```

## Configuration Scopes

| Scope | Storage Location | Use Case |
|-------|------------------|----------|
| `user` | `~/.claude/settings.json` | Global across all projects |
| `local` | `.claude/settings.json` | Project-specific |
| `project` | `.claude/settings.json` (committed) | Team-shared |

## ChromaDB Collections

Auto-created collections for different content types:
- `mcp_conversations` - Chat memory and context
- `mcp_code_snippets` - Code patterns and examples
- `mcp_documentation` - Project docs and guides
- `mcp_github_content` - Repository content
- `mcp_file_content` - File system content

## Integration Workflow

```mermaid
sequenceDiagram
    participant User
    participant Claude
    participant MCP
    participant Memory
    participant ChromaDB
    participant TimescaleDB

    User->>Claude: "Remember this conversation"
    Claude->>MCP: Route to memory
    MCP->>Memory: Store conversation
    Memory->>ChromaDB: Create embedding
    Memory->>TimescaleDB: Store metadata
    TimescaleDB-->>Memory: Success
    ChromaDB-->>Memory: Vector ID
    Memory-->>Claude: Stored
    Claude-->>User: "Conversation remembered"
```

## Performance Monitoring

```bash
# Create monitoring script
cat > ~/.claude/monitor.sh << 'EOF'
#!/bin/bash
echo "=== MCP Status ==="
claude mcp list

echo -e "\n=== Databases ==="
curl -s http://localhost:6333/health > /dev/null && echo "✓ Qdrant" || echo "✗ Qdrant"
psql "$DATABASE_URL" -c "SELECT 1;" > /dev/null 2>&1 && echo "✓ PostgreSQL" || echo "✗ PostgreSQL"

echo -e "\n=== Storage ==="
echo "ChromaDB: $(du -sh ~/.chromadb/ 2>/dev/null | cut -f1)"
echo "Claude Config: $(du -sh ~/.claude/ 2>/dev/null | cut -f1)"

echo -e "\n=== Containers ==="
docker ps --filter "name=qicore" --format "table {{.Names}}\t{{.Status}}"
EOF

chmod +x ~/.claude/monitor.sh
```

## Installation Notes

### Tools Required
- **UV/UVX**: For Qdrant MCP server (Python package manager)
- **NPX**: For most MCP servers (comes with Node.js)
- **Docker**: For TimescaleDB, Qdrant, Redis containers

### Important Steps
1. **Always run `source ~/.zshrc`** after installing UV
2. **Use `--scope user`** for global MCP server access
3. **API keys required** for GitHub and Brave Search servers
4. **TimescaleDB schemas** must be created before MCP servers

### Why This Architecture?

- **TimescaleDB**: Optimal for time-series data (operations, logs, metrics)
- **ChromaDB**: Perfect for local development and persistent memory
- **Qdrant**: Best for high-performance vector operations and production scale
- **Integrated approach**: Each MCP server stores data in appropriate database
- **Semantic capabilities**: Code similarity, conversation memory, documentation search

This setup provides the most comprehensive Claude Code development environment possible in 2025.