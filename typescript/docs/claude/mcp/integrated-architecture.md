# Integrated MCP Architecture with TimescaleDB and ChromaDB

## Overview

This document describes the comprehensive MCP (Model Context Protocol) architecture that integrates:
- **TimescaleDB** (PostgreSQL) for structured data and time-series storage
- **ChromaDB** for vector embeddings and semantic search
- **Docker containers** for service management
- **8 MCP servers** for complete development workflow coverage

## Architecture Diagram (ASCII)

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                            Claude Code + MCP Servers                            │
├─────────────────────────────────────────────────────────────────────────────────┤
│  Sequential  │  Memory   │  Qdrant   │  Brave    │  Context7  │  GitHub  │ FS │ │
│  Thinking    │  Server   │  Vector   │  Search   │  Docs      │  Ops     │Ops │ │
└──────┬───────┴─────┬─────┴─────┬─────┴─────┬─────┴─────┬──────┴─────┬────┴────┬┘
       │             │           │           │           │            │         │
       │             │           │           │           │            │         │
       v             v           v           v           v            v         v
┌─────────────┐ ┌──────────────────────────────┐ ┌────────────────┐ ┌─────────────┐
│  ChromaDB   │ │        TimescaleDB          │ │   Qdrant       │ │   Docker    │
│   MCP       │ │     (qicore-timescaledb)    │ │(qicore-qdrant) │ │   Engine    │
│  Server     │ │                             │ │                │ │             │
└─────────────┘ └──────────────────────────────┘ └────────────────┘ └─────────────┘
       │                         │                         │               │
       │                         │                         │               │
       v                         v                         v               v
┌─────────────┐ ┌──────────────────────────────┐ ┌────────────────┐ ┌─────────────┐
│ .chromadb/  │ │      PostgreSQL DB           │ │  Vector Store  │ │ Containers  │
│ (local)     │ │   - Structured Data          │ │  - Embeddings  │ │ - TimescaleDB│
│ - Embeddings│ │   - Time Series              │ │  - Semantic    │ │ - Qdrant     │
│ - Metadata  │ │   - Relational               │ │    Search      │ │ - Redis      │
│ - Collections│ │   - MCP Schemas             │ │                │ │ - RedPanda   │
└─────────────┘ └──────────────────────────────┘ └────────────────┘ └─────────────┘
```

## Architecture Diagram (Mermaid)

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
    DOCKER --> CONT4[qicore-redpanda]

    classDef mcpServer fill:#e1f5fe,stroke:#01579b,stroke-width:2px
    classDef database fill:#f3e5f5,stroke:#4a148c,stroke-width:2px
    classDef storage fill:#e8f5e8,stroke:#1b5e20,stroke-width:2px
    classDef container fill:#fff3e0,stroke:#e65100,stroke-width:2px

    class ST,MEM,QD,CHR,BS,CTX,GH,FS,DK,PG mcpServer
    class TSC,PGDB,QDB,CDB database
    class CONT1,CONT2,CONT3,CONT4 container
```

### Data Flow Architecture

```mermaid
flowchart LR
    User[User Input] --> Claude[Claude Code]
    Claude --> Router{MCP Router}

    Router -->|AI Reasoning| ST[Sequential Thinking]
    Router -->|Memory Ops| MEM[Memory Server]
    Router -->|Vector Search| Vector{Vector Choice}
    Router -->|Web Search| BS[Brave Search]
    Router -->|Code Docs| CTX[Context7]
    Router -->|Git Ops| GH[GitHub MCP]
    Router -->|File Ops| FS[File System]
    Router -->|Container Ops| DK[Docker MCP]
    Router -->|DB Queries| PG[PostgreSQL MCP]

    Vector -->|High Performance| QD[Qdrant MCP]
    Vector -->|Local/Dev| CHR[ChromaDB MCP]

    ST --> Memory[(Persistent Memory)]
    MEM --> Memory
    QD --> QDB[(Qdrant Vector Store)]
    CHR --> CDB[(ChromaDB Local)]
    GH --> STRUCT[(Structured Data)]
    FS --> STRUCT
    DK --> STRUCT
    PG --> STRUCT

    Memory --> TimeSeries[(TimescaleDB)]
    STRUCT --> TimeSeries

    TimeSeries --> Analytics[Analytics & Insights]
    Memory --> Semantic[Semantic Search]

    classDef user fill:#e3f2fd,stroke:#0d47a1,stroke-width:3px
    classDef core fill:#e8f5e8,stroke:#2e7d32,stroke-width:2px
    classDef mcp fill:#fff3e0,stroke:#f57c00,stroke-width:2px
    classDef data fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
    classDef output fill:#ffebee,stroke:#c62828,stroke-width:2px

    class User user
    class Claude,Router core
    class ST,MEM,QD,CHR,BS,CTX,GH,FS,DK,PG mcp
    class Memory,QDB,CDB,STRUCT,TimeSeries data
    class Analytics,Semantic output
```

### Database Integration Schema

```mermaid
erDiagram
    Claude_Code ||--o{ MCP_Servers : manages
    MCP_Servers ||--o{ TimescaleDB : writes_to
    MCP_Servers ||--o{ ChromaDB : embeds_to
    MCP_Servers ||--o{ Qdrant : vectors_to

    TimescaleDB {
        schema mcp_core
        schema mcp_memory
        schema mcp_github
        schema mcp_filesystem
        schema mcp_docker
    }

    ChromaDB {
        collection mcp_conversations
        collection mcp_code_snippets
        collection mcp_documentation
        collection mcp_errors_solutions
        collection mcp_github_content
        collection mcp_file_content
    }

    Qdrant {
        collection mcp-memories
        index vector_index
        metadata filters
    }

    MCP_Servers {
        string name
        string type
        string status
        timestamp last_health_check
        json configuration
    }
```

## Database Responsibilities

### TimescaleDB (PostgreSQL)
- **Structured Data**: User settings, configurations, logs
- **Time Series**: Operations history, performance metrics
- **Relational Data**: GitHub repos, issues, file operations
- **MCP Metadata**: Server health, connection logs

### ChromaDB
- **Vector Embeddings**: Document embeddings, semantic search
- **Persistent Memory**: Conversation context, code snippets
- **Knowledge Base**: Documentation, best practices
- **Similarity Search**: Code patterns, error solutions

### Qdrant
- **High-Performance Vectors**: Large-scale embeddings
- **Production Vector Search**: Complex similarity queries
- **Distributed Storage**: Scalable vector operations

## Installation Commands

### 1. TimescaleDB Integration (Already Done)

```bash
# Verify TimescaleDB container
docker ps | grep timescale
# Should show: qicore-timescaledb (healthy)

# Check database connection
psql "postgresql://postgres:password@localhost:5432/cryptodb" -c "SELECT version();"
```

### 2. ChromaDB MCP Server Installation

```bash
# Install ChromaDB MCP Server (Official Implementation)
claude mcp add-json --scope user "chromadb" '{"command":"npx","args":["-y","chroma-mcp-server"],"env":{"CHROMA_PATH":"/home/zzhang/.chromadb","CHROMA_EMBEDDING_FUNCTION":"default","CHROMA_PERSIST_DIRECTORY":"/home/zzhang/.chromadb/data"}}'

# Alternative: Python implementation
# uvx chroma-mcp-server --chroma-path ~/.chromadb
```

### 3. Core MCP Servers with Database Integration

```bash
# PostgreSQL MCP (Database Operations)
claude mcp add-json --scope user "postgresql" '{"command":"npx","args":["-y","@modelcontextprotocol/server-postgres"],"env":{"DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb","POSTGRES_HOST":"localhost","POSTGRES_PORT":"5432","POSTGRES_USER":"postgres","POSTGRES_PASSWORD":"password","POSTGRES_DATABASE":"cryptodb"}}'

# Docker MCP (Container Management)
claude mcp add-json --scope user "docker" '{"command":"npx","args":["-y","@modelcontextprotocol/server-docker"],"env":{"DOCKER_HOST":"unix:///var/run/docker.sock"}}'

# GitHub MCP (with TimescaleDB storage)
claude mcp add-json --scope user "github" '{"command":"npx","args":["-y","@modelcontextprotocol/server-github"],"env":{"GITHUB_PERSONAL_ACCESS_TOKEN":"your-github-token","DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'

# File System MCP (with operation logging)
claude mcp add-json --scope user "filesystem" '{"command":"npx","args":["-y","@modelcontextprotocol/server-filesystem"],"env":{"ALLOWED_DIRECTORIES":"/home/zzhang/dev,/home/zzhang/dev/qi,/home/zzhang/.claude","DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'
```

## Database Schema Setup

### TimescaleDB Schema Creation

```sql
-- Connect to TimescaleDB
psql "postgresql://postgres:password@localhost:5432/cryptodb"

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

-- GitHub operations and metadata
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
    operation_type VARCHAR(50) NOT NULL, -- read, write, create, delete, search
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
    action VARCHAR(50) NOT NULL, -- start, stop, restart, inspect, logs
    status VARCHAR(50),
    exit_code INTEGER,
    output TEXT,
    executed_at TIMESTAMPTZ DEFAULT NOW()
);

-- Create TimescaleDB hypertables for time-series data
SELECT create_hypertable('mcp_memory.conversations', 'created_at', if_not_exists => TRUE);
SELECT create_hypertable('mcp_filesystem.operations', 'executed_at', if_not_exists => TRUE);
SELECT create_hypertable('mcp_docker.container_logs', 'executed_at', if_not_exists => TRUE);

-- Create indexes for better query performance
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
```

### ChromaDB Setup

```bash
# Create ChromaDB directories
mkdir -p ~/.chromadb/data
mkdir -p ~/.chromadb/collections

# Set permissions
chmod 755 ~/.chromadb
chmod 755 ~/.chromadb/data
chmod 755 ~/.chromadb/collections

# Create collections for different purposes
cat > ~/.chromadb/init_collections.py << 'EOF'
import chromadb
from chromadb.config import Settings

# Initialize ChromaDB client
client = chromadb.PersistentClient(path="~/.chromadb/data")

# Create collections for different MCP use cases
collections = [
    "mcp_conversations",    # Conversation memory and context
    "mcp_code_snippets",   # Code examples and patterns
    "mcp_documentation",   # Project documentation and guides
    "mcp_errors_solutions", # Error patterns and solutions
    "mcp_github_content",  # GitHub repository content
    "mcp_file_content"     # File system content for search
]

for collection_name in collections:
    try:
        collection = client.create_collection(name=collection_name)
        print(f"Created collection: {collection_name}")
    except Exception as e:
        print(f"Collection {collection_name} already exists or error: {e}")

print("ChromaDB collections initialized successfully!")
EOF

# Run initialization (optional - collections created automatically)
python3 ~/.chromadb/init_collections.py
```

## Environment Configuration

### Update ~/.zshrc

```bash
cat >> ~/.zshrc << 'EOF'

# =============================================================================
# MCP Integrated Architecture Configuration
# =============================================================================

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
export CHROMA_COLLECTION_NAME="mcp_conversations"

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

# MCP Server Configuration
export MCP_EMBEDDING_PROVIDER="chromadb"  # or "qdrant" for high-performance
export MCP_PRIMARY_DATABASE="timescaledb"
export MCP_VECTOR_DATABASE="chromadb"

EOF

# Reload configuration
source ~/.zshrc
```

## Complete MCP Server List

After setup, you should have **9 MCP servers**:

```bash
❯ claude mcp list
Checking MCP server health...

qdrant: uvx mcp-server-qdrant - ✓ Connected
memory: npx -y mcp-memory-server - ✓ Connected
brave-search: npx -y @brave/brave-search-mcp-server - ✓ Connected
context7: npx -y @upstash/context7-mcp@latest - ✓ Connected
sequential-thinking: npx -y @modelcontextprotocol/server-sequential-thinking - ✓ Connected
chromadb: npx -y chroma-mcp-server - ✓ Connected
postgresql: npx -y @modelcontextprotocol/server-postgres - ✓ Connected
github: npx -y @modelcontextprotocol/server-github - ✓ Connected
filesystem: npx -y @modelcontextprotocol/server-filesystem - ✓ Connected
docker: npx -y @modelcontextprotocol/server-docker - ✓ Connected
```

## Integrated Workflow Examples

### 1. Complete Development Workflow
```bash
# Claude can now:
# 1. Check Docker container status
# 2. Read project files
# 3. Store insights in ChromaDB
# 4. Log operations in TimescaleDB
# 5. Manage GitHub repositories
# 6. Perform semantic searches
```

### 2. Memory and Context Management
```bash
# Conversation: "Remember this error pattern and solution"
# Flow: Sequential Thinking → ChromaDB (embedding) → TimescaleDB (metadata)
```

### 3. Repository Analysis
```bash
# Conversation: "Analyze this GitHub repo and remember key patterns"
# Flow: GitHub MCP → File System → ChromaDB (embeddings) → TimescaleDB (structure)
```

### 4. Container + Database Operations
```bash
# Conversation: "Check TimescaleDB health and query recent operations"
# Flow: Docker MCP → PostgreSQL MCP → TimescaleDB query results
```

## Verification and Testing

### System Health Check
```bash
# Check all services
docker ps --filter "name=qicore"
curl -s http://localhost:6333/health  # Qdrant
ls -la ~/.chromadb/data               # ChromaDB
psql "$DATABASE_URL" -c "SELECT version();" # TimescaleDB

# Check MCP servers
claude mcp list

# Test integrated workflow
claude
# Try: "Show me Docker container status, then query the database for recent operations"
```

### Database Content Verification
```sql
-- Check TimescaleDB schemas
SELECT schema_name FROM information_schema.schemata WHERE schema_name LIKE 'mcp_%';

-- Check ChromaDB collections
-- (Use ChromaDB client or web interface)
```

## Maintenance and Monitoring

### Daily Operations
- Monitor Docker containers: `docker ps`
- Check MCP server health: `claude mcp list`
- Review database logs: `docker logs qicore-timescaledb`
- ChromaDB storage usage: `du -sh ~/.chromadb/`

### Performance Optimization
- **TimescaleDB**: Regular maintenance, index optimization
- **ChromaDB**: Collection management, embedding updates
- **Qdrant**: Vector index optimization
- **MCP Servers**: Connection pooling, timeout configuration

## Troubleshooting

### Common Issues
1. **ChromaDB permission errors**: Check `~/.chromadb/` ownership
2. **TimescaleDB connection issues**: Verify Docker container health
3. **MCP server failures**: Check environment variables
4. **Vector search performance**: Monitor embedding dimensions

### Recovery Procedures
1. **Container restart**: `docker restart qicore-timescaledb qicore-qdrant`
2. **ChromaDB reset**: Delete `~/.chromadb/data/` and reinitialize
3. **MCP server restart**: Remove and re-add problematic servers
4. **Database schema recreation**: Re-run schema setup SQL

This integrated architecture provides a complete AI development platform with persistent memory, structured data storage, vector search capabilities, and seamless container management.