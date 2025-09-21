# Complete MCP Installation Commands - Integrated Architecture

## Overview

This document provides the complete installation commands for the integrated MCP architecture with:
- **10 MCP Servers** (including ChromaDB and PostgreSQL)
- **TimescaleDB** integration for structured data
- **ChromaDB** integration for vector embeddings
- **Qdrant** for high-performance vector operations
- **Docker** container management

## Installation Flow Diagram

```mermaid
flowchart TD
    Start[Start Installation] --> Prereq[Check Prerequisites]
    Prereq --> Tools{Tools Available?}
    Tools -->|No| Install[Install Missing Tools]
    Tools -->|Yes| Env[Setup Environment]
    Install --> Env

    Env --> DB[Setup Databases]
    DB --> TSSetup[TimescaleDB Schema]
    DB --> ChromaSetup[ChromaDB Directories]

    TSSetup --> MCPCore[Install Core MCP Servers]
    ChromaSetup --> MCPCore

    MCPCore --> Memory[Memory + Sequential]
    MCPCore --> Vector[Vector Databases]
    MCPCore --> External[External Services]
    MCPCore --> DevTools[Development Tools]
    MCPCore --> Database[Database Operations]

    Memory --> ST[Sequential Thinking]
    Memory --> MEM[Memory Server]

    Vector --> QD[Qdrant MCP]
    Vector --> CHR[ChromaDB MCP]

    External --> BS[Brave Search]
    External --> CTX[Context7]

    DevTools --> GH[GitHub MCP]
    DevTools --> FS[File System MCP]
    DevTools --> DK[Docker MCP]

    Database --> PG[PostgreSQL MCP]

    ST --> Verify[Verification]
    MEM --> Verify
    QD --> Verify
    CHR --> Verify
    BS --> Verify
    CTX --> Verify
    GH --> Verify
    FS --> Verify
    DK --> Verify
    PG --> Verify

    Verify --> Test[Integration Testing]
    Test --> Success[🎉 Complete Setup]

    classDef start fill:#e8f5e8,stroke:#2e7d32,stroke-width:3px
    classDef setup fill:#e3f2fd,stroke:#1565c0,stroke-width:2px
    classDef mcp fill:#fff3e0,stroke:#f57c00,stroke-width:2px
    classDef success fill:#e8f5e8,stroke:#388e3c,stroke-width:3px

    class Start,Success start
    class Prereq,Tools,Install,Env,DB,TSSetup,ChromaSetup setup
    class MCPCore,Memory,Vector,External,DevTools,Database,ST,MEM,QD,CHR,BS,CTX,GH,FS,DK,PG mcp
    class Verify,Test success
```

## Prerequisites Verification

```bash
# Check all prerequisites
echo "=== Prerequisites Check ==="
node --version          # Should be 18+
python3 --version       # Should be 3.8+
docker --version        # Should show version
uv --version            # Should show version (if not, install UV first)

# Check Docker services
docker ps | grep -E "(timescale|qdrant|redis)"
# Should show: qicore-timescaledb, qicore-qdrant, qicore-redis

# Check database connectivity
curl -s http://localhost:6333/health && echo " ✓ Qdrant OK"
psql "postgresql://postgres:password@localhost:5432/cryptodb" -c "SELECT 1;" && echo "✓ PostgreSQL OK"
```

## Environment Setup

```bash
# Add all environment variables to ~/.zshrc
cat >> ~/.zshrc << 'EOF'

# =============================================================================
# Integrated MCP Architecture Configuration
# =============================================================================

# Core Paths
export PATH="$HOME/.cargo/bin:$PATH"

# Database Configuration
export DATABASE_URL="postgresql://postgres:password@localhost:5432/cryptodb"
export POSTGRES_HOST="localhost"
export POSTGRES_PORT="5432"
export POSTGRES_USER="postgres"
export POSTGRES_PASSWORD="password"
export POSTGRES_DATABASE="cryptodb"

# ChromaDB Configuration
export CHROMA_PATH="/home/zzhang/.chromadb"
export CHROMA_PERSIST_DIRECTORY="/home/zzhang/.chromadb/data"
export CHROMA_EMBEDDING_FUNCTION="default"
export CHROMA_HOST="localhost"
export CHROMA_PORT="8000"

# Qdrant Configuration
export QDRANT_URL="http://localhost:6333"
export QDRANT_AUTO_CREATE_COLLECTION=true
export COLLECTION_NAME="mcp-memories"
export EMBEDDING_MODEL="sentence-transformers/all-MiniLM-L6-v2"

# GitHub Configuration
export GITHUB_PERSONAL_ACCESS_TOKEN="your-github-token-here"

# File System MCP Configuration
export ALLOWED_DIRECTORIES="/home/zzhang/dev,/home/zzhang/dev/qi,/home/zzhang/.claude"

# Docker Configuration
export DOCKER_HOST="unix:///var/run/docker.sock"

# API Keys
export BRAVE_API_KEY="your-brave-api-key-here"

EOF

# Reload configuration
source ~/.zshrc
```

## Database Setup

### 1. TimescaleDB Schema Creation

```bash
# Create comprehensive MCP schemas in TimescaleDB
psql "postgresql://postgres:password@localhost:5432/cryptodb" << 'EOF'

-- Create MCP schemas
CREATE SCHEMA IF NOT EXISTS mcp_core;
CREATE SCHEMA IF NOT EXISTS mcp_memory;
CREATE SCHEMA IF NOT EXISTS mcp_github;
CREATE SCHEMA IF NOT EXISTS mcp_filesystem;
CREATE SCHEMA IF NOT EXISTS mcp_docker;

-- Core MCP server tracking
CREATE TABLE IF NOT EXISTS mcp_core.servers (
    id SERIAL PRIMARY KEY,
    server_name VARCHAR(255) UNIQUE NOT NULL,
    server_type VARCHAR(100) NOT NULL,
    status VARCHAR(50) DEFAULT 'active',
    last_health_check TIMESTAMPTZ DEFAULT NOW(),
    configuration JSONB,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Memory and conversation storage
CREATE TABLE IF NOT EXISTS mcp_memory.conversations (
    id SERIAL PRIMARY KEY,
    session_id UUID NOT NULL,
    content TEXT NOT NULL,
    content_type VARCHAR(50) DEFAULT 'text',
    embedding_source VARCHAR(50) DEFAULT 'chromadb',
    metadata JSONB,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- GitHub operations
CREATE TABLE IF NOT EXISTS mcp_github.repositories (
    id SERIAL PRIMARY KEY,
    owner VARCHAR(255) NOT NULL,
    name VARCHAR(255) NOT NULL,
    full_name VARCHAR(255) UNIQUE NOT NULL,
    description TEXT,
    last_accessed TIMESTAMPTZ DEFAULT NOW(),
    metadata JSONB,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS mcp_github.issues (
    id SERIAL PRIMARY KEY,
    repo_id INTEGER REFERENCES mcp_github.repositories(id),
    issue_number INTEGER NOT NULL,
    title TEXT NOT NULL,
    state VARCHAR(20) NOT NULL,
    body TEXT,
    labels JSONB,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE(repo_id, issue_number)
);

-- File system operations
CREATE TABLE IF NOT EXISTS mcp_filesystem.operations (
    id SERIAL PRIMARY KEY,
    operation_type VARCHAR(50) NOT NULL,
    file_path TEXT NOT NULL,
    file_size BIGINT,
    success BOOLEAN DEFAULT TRUE,
    error_message TEXT,
    duration_ms INTEGER,
    executed_at TIMESTAMPTZ DEFAULT NOW()
);

-- Docker container management
CREATE TABLE IF NOT EXISTS mcp_docker.container_logs (
    id SERIAL PRIMARY KEY,
    container_name VARCHAR(255) NOT NULL,
    container_id VARCHAR(255) NOT NULL,
    image_name VARCHAR(255),
    action VARCHAR(50) NOT NULL,
    status VARCHAR(50),
    exit_code INTEGER,
    output TEXT,
    executed_at TIMESTAMPTZ DEFAULT NOW()
);

-- Create TimescaleDB hypertables
SELECT create_hypertable('mcp_memory.conversations', 'created_at', if_not_exists => TRUE);
SELECT create_hypertable('mcp_filesystem.operations', 'executed_at', if_not_exists => TRUE);
SELECT create_hypertable('mcp_docker.container_logs', 'executed_at', if_not_exists => TRUE);

-- Create indexes
CREATE INDEX IF NOT EXISTS idx_conversations_session ON mcp_memory.conversations(session_id);
CREATE INDEX IF NOT EXISTS idx_repositories_owner_name ON mcp_github.repositories(owner, name);
CREATE INDEX IF NOT EXISTS idx_filesystem_ops_type ON mcp_filesystem.operations(operation_type);
CREATE INDEX IF NOT EXISTS idx_docker_logs_container ON mcp_docker.container_logs(container_name);

-- Grant permissions
GRANT ALL PRIVILEGES ON SCHEMA mcp_core TO postgres;
GRANT ALL PRIVILEGES ON SCHEMA mcp_memory TO postgres;
GRANT ALL PRIVILEGES ON SCHEMA mcp_github TO postgres;
GRANT ALL PRIVILEGES ON SCHEMA mcp_filesystem TO postgres;
GRANT ALL PRIVILEGES ON SCHEMA mcp_docker TO postgres;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA mcp_core TO postgres;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA mcp_memory TO postgres;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA mcp_github TO postgres;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA mcp_filesystem TO postgres;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA mcp_docker TO postgres;

EOF
```

### 2. ChromaDB Directory Setup

```bash
# Create ChromaDB directory structure
mkdir -p ~/.chromadb/{data,collections,embeddings,logs}
chmod 755 ~/.chromadb
chmod 755 ~/.chromadb/data
chmod 755 ~/.chromadb/collections
```

## Complete MCP Server Installation

**Important**: Run these commands in order, **one at a time** to ensure proper installation.

### 1. Core Memory and Vector Servers

```bash
# Sequential Thinking (AI reasoning)
claude mcp add-json --scope user "sequential-thinking" '{"command":"npx","args":["-y","@modelcontextprotocol/server-sequential-thinking"]}'

# Memory Server (PostgreSQL + vector storage)
claude mcp add-json --scope user "memory" '{"command":"npx","args":["-y","mcp-memory-server"],"env":{"DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'

# Qdrant Vector Database (high-performance vectors)
claude mcp add-json --scope user "qdrant" '{"command":"uvx","args":["mcp-server-qdrant"],"env":{"QDRANT_URL":"http://localhost:6333","COLLECTION_NAME":"mcp-memories","EMBEDDING_MODEL":"sentence-transformers/all-MiniLM-L6-v2"}}'

# ChromaDB (local vector storage and embeddings)
claude mcp add-json --scope user "chromadb" '{"command":"npx","args":["-y","chroma-mcp-server"],"env":{"CHROMA_PATH":"/home/zzhang/.chromadb","CHROMA_EMBEDDING_FUNCTION":"default","CHROMA_PERSIST_DIRECTORY":"/home/zzhang/.chromadb/data"}}'
```

### 2. External Service Integration

```bash
# Brave Search (web search capabilities)
claude mcp add-json --scope user "brave-search" '{"command":"npx","args":["-y","@brave/brave-search-mcp-server"],"env":{"BRAVE_API_KEY":"your-brave-api-key-here"}}'

# Context7 (live code documentation)
claude mcp add-json --scope user "context7" '{"command":"npx","args":["-y","@upstash/context7-mcp@latest"]}'
```

### 3. Development Tool Integration

```bash
# GitHub (repository management)
claude mcp add-json --scope user "github" '{"command":"npx","args":["-y","@modelcontextprotocol/server-github"],"env":{"GITHUB_PERSONAL_ACCESS_TOKEN":"your-github-token-here","DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'

# File System (local file operations)
claude mcp add-json --scope user "filesystem" '{"command":"npx","args":["-y","@modelcontextprotocol/server-filesystem"],"env":{"ALLOWED_DIRECTORIES":"/home/zzhang/dev,/home/zzhang/dev/qi,/home/zzhang/.claude","DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'

# Docker (container management)
claude mcp add-json --scope user "docker" '{"command":"npx","args":["-y","@modelcontextprotocol/server-docker"],"env":{"DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'
```

### 4. Database Operations

```bash
# PostgreSQL (direct database operations)
claude mcp add-json --scope user "postgresql" '{"command":"npx","args":["-y","@modelcontextprotocol/server-postgres"],"env":{"DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb","POSTGRES_HOST":"localhost","POSTGRES_PORT":"5432","POSTGRES_USER":"postgres","POSTGRES_PASSWORD":"password","POSTGRES_DATABASE":"cryptodb"}}'
```

## Verification and Testing

### 1. Check All MCP Servers

```bash
# Should show 10 connected servers
claude mcp list
```

**Expected Output:**
```
Checking MCP server health...

sequential-thinking: npx -y @modelcontextprotocol/server-sequential-thinking - ✓ Connected
memory: npx -y mcp-memory-server - ✓ Connected
qdrant: uvx mcp-server-qdrant - ✓ Connected
chromadb: npx -y chroma-mcp-server - ✓ Connected
brave-search: npx -y @brave/brave-search-mcp-server - ✓ Connected
context7: npx -y @upstash/context7-mcp@latest - ✓ Connected
github: npx -y @modelcontextprotocol/server-github - ✓ Connected
filesystem: npx -y @modelcontextprotocol/server-filesystem - ✓ Connected
docker: npx -y @modelcontextprotocol/server-docker - ✓ Connected
postgresql: npx -y @modelcontextprotocol/server-postgres - ✓ Connected
```

### 2. Database Verification

```bash
# Check TimescaleDB schemas
psql "postgresql://postgres:password@localhost:5432/cryptodb" -c "SELECT schema_name FROM information_schema.schemata WHERE schema_name LIKE 'mcp_%';"

# Check ChromaDB
ls -la ~/.chromadb/data/

# Check Qdrant
curl -s http://localhost:6333/health

# Check Docker containers
docker ps --filter "name=qicore"
```

### 3. Integration Test

```bash
# Start Claude and test integrated capabilities
claude

# Test commands to try:
# "Show me Docker container status"
# "Search my GitHub repositories"
# "Store this conversation for future reference"
# "What files are in my dev directory?"
# "Search the web for TimescaleDB documentation"
```

## API Keys Required

### 1. GitHub Personal Access Token
```bash
# Get from: https://github.com/settings/personal-access-tokens
# Scopes needed: repo, issues, pull_requests, actions
# Replace "your-github-token-here" in environment variables
```

### 2. Brave Search API Key
```bash
# Get from: https://api.search.brave.com/app/keys
# Replace "your-brave-api-key-here" in environment variables
```

## Troubleshooting Quick Fixes

### Common Issues

```bash
# 1. UV/UVX not found
source ~/.zshrc

# 2. Database connection issues
docker restart qicore-timescaledb qicore-qdrant

# 3. ChromaDB permission errors
chmod -R 755 ~/.chromadb/

# 4. MCP server startup failures
claude mcp list  # Check which servers failed
# Remove and re-add failed servers

# 5. Port conflicts
lsof -i :6333  # Qdrant
lsof -i :5432  # PostgreSQL
lsof -i :8000  # ChromaDB
```

### Recovery Commands

```bash
# Remove all MCP servers (if needed)
for server in sequential-thinking memory qdrant chromadb brave-search context7 github filesystem docker postgresql; do
    claude mcp remove $server 2>/dev/null || true
done

# Then re-run installation commands above
```

## Performance Monitoring

```bash
# Create monitoring script
cat > ~/.claude/monitor_mcp.sh << 'EOF'
#!/bin/bash
echo "=== MCP Architecture Status ==="
echo "Date: $(date)"
echo

echo "📊 MCP Servers:"
claude mcp list

echo -e "\n🐳 Docker Containers:"
docker ps --filter "name=qicore" --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"

echo -e "\n💾 Database Health:"
curl -s http://localhost:6333/health > /dev/null && echo "✓ Qdrant: Healthy" || echo "✗ Qdrant: Failed"
psql "postgresql://postgres:password@localhost:5432/cryptodb" -c "SELECT 1;" > /dev/null 2>&1 && echo "✓ PostgreSQL: Healthy" || echo "✗ PostgreSQL: Failed"

echo -e "\n📁 Storage Usage:"
echo "ChromaDB: $(du -sh ~/.chromadb/ 2>/dev/null | cut -f1)"
echo "Claude Config: $(du -sh ~/.claude/ 2>/dev/null | cut -f1)"

echo -e "\n🔧 Environment Variables:"
echo "DATABASE_URL: ${DATABASE_URL:0:30}..."
echo "CHROMA_PATH: $CHROMA_PATH"
echo "QDRANT_URL: $QDRANT_URL"
echo "GITHUB_TOKEN: ${GITHUB_PERSONAL_ACCESS_TOKEN:0:8}..."
EOF

chmod +x ~/.claude/monitor_mcp.sh
```

## Success Criteria

✅ **10 MCP servers connected**
✅ **TimescaleDB schemas created**
✅ **ChromaDB collections initialized**
✅ **Qdrant vector database accessible**
✅ **Docker containers healthy**
✅ **Environment variables configured**
✅ **API keys set (GitHub, Brave Search)**
✅ **Integration tests passing**

This complete installation creates the most comprehensive Claude Code MCP setup available in 2025, providing AI-powered development capabilities with persistent memory, vector search, database operations, container management, and external service integration.