# ChromaDB MCP Server Setup and Integration

## Overview

ChromaDB provides vector database capabilities for semantic search, document embeddings, and persistent memory. Multiple MCP servers require ChromaDB for optimal functionality, including memory servers, conversation storage, and code analysis tools.

## ChromaDB MCP Server Options

Based on 2025 implementations, there are several ChromaDB MCP servers available:

### ChromaDB Architecture Overview

```mermaid
graph TB
    subgraph "MCP Servers Requiring ChromaDB"
        MEM[Memory Server]
        ST[Sequential Thinking]
        GH[GitHub Content]
        FS[File System Search]
    end

    subgraph "ChromaDB Implementations"
        OFF[Official Chroma MCP]
        HUM[HumainLabs MCP]
        COM[Community MCPs]
    end

    subgraph "ChromaDB Core"
        EMB[Embedding Functions]
        COL[Collections]
        VEC[Vector Store]
        META[Metadata]
    end

    subgraph "Storage Backends"
        LOCAL[Local Persistent]
        DOCKER[Docker Container]
        CLOUD[Chroma Cloud]
    end

    MEM --> OFF
    ST --> OFF
    GH --> OFF
    FS --> OFF

    OFF --> EMB
    HUM --> EMB
    COM --> EMB

    EMB --> COL
    COL --> VEC
    VEC --> META

    VEC --> LOCAL
    VEC --> DOCKER
    VEC --> CLOUD

    classDef mcpServer fill:#e1f5fe,stroke:#01579b,stroke-width:2px
    classDef chromaImpl fill:#f3e5f5,stroke:#4a148c,stroke-width:2px
    classDef chromaCore fill:#e8f5e8,stroke:#1b5e20,stroke-width:2px
    classDef storage fill:#fff3e0,stroke:#e65100,stroke-width:2px

    class MEM,ST,GH,FS mcpServer
    class OFF,HUM,COM chromaImpl
    class EMB,COL,VEC,META chromaCore
    class LOCAL,DOCKER,CLOUD storage
```

### 1. Official Chroma Implementation (Recommended)
- **Repository**: `chroma-core/chroma-mcp`
- **Features**: Full MCP integration, official support
- **Installation**: `chroma-mcp-server` npm package

### 2. HumainLabs Implementation
- **Repository**: `HumainLabs/chromaDB-mcp`
- **Features**: Enhanced vector capabilities, metadata filtering
- **Installation**: Custom installation process

### 3. Community Implementations
- **Multiple providers**: Various community-maintained servers
- **Features**: Specialized use cases, different embedding functions

## Installation Methods

### Method 1: Official ChromaDB MCP Server (NPX)

```bash
# Install official ChromaDB MCP server
claude mcp add-json --scope user "chromadb" '{"command":"npx","args":["-y","chroma-mcp-server"],"env":{"CHROMA_PATH":"/home/zzhang/.chromadb","CHROMA_EMBEDDING_FUNCTION":"default","CHROMA_PERSIST_DIRECTORY":"/home/zzhang/.chromadb/data","CHROMA_HOST":"localhost","CHROMA_PORT":"8000"}}'
```

### Method 2: Python Implementation (UVX)

```bash
# Install via uvx (Python)
claude mcp add-json --scope user "chromadb-py" '{"command":"uvx","args":["chroma-mcp-server","--chroma-path","/home/zzhang/.chromadb"],"env":{"CHROMA_PERSIST_DIRECTORY":"/home/zzhang/.chromadb/data","CHROMA_EMBEDDING_FUNCTION":"sentence-transformers/all-MiniLM-L6-v2"}}'
```

### Method 3: Docker ChromaDB + MCP Bridge

```bash
# Start ChromaDB in Docker (alternative to local)
docker run -d \
  --name chromadb \
  -p 8000:8000 \
  -v ~/.chromadb:/chroma/chroma \
  chromadb/chroma:latest

# Add MCP server that connects to Docker ChromaDB
claude mcp add-json --scope user "chromadb-docker" '{"command":"npx","args":["-y","chroma-mcp-server"],"env":{"CHROMA_HOST":"localhost","CHROMA_PORT":"8000","CHROMA_PERSIST_DIRECTORY":"/chroma/chroma"}}'
```

## Directory Structure Setup

```bash
# Create ChromaDB directory structure
mkdir -p ~/.chromadb/{data,collections,embeddings,logs}

# Set proper permissions
chmod 755 ~/.chromadb
chmod 755 ~/.chromadb/data
chmod 755 ~/.chromadb/collections

# Create configuration file
cat > ~/.chromadb/chroma_config.yaml << 'EOF'
# ChromaDB Configuration for MCP Integration
persist_directory: "/home/zzhang/.chromadb/data"
anonymized_telemetry: false
allow_reset: true
default_embedding_function: "sentence-transformers/all-MiniLM-L6-v2"
collections:
  mcp_conversations:
    description: "Conversation memory and context"
    embedding_function: "default"
  mcp_code_snippets:
    description: "Code examples and patterns"
    embedding_function: "default"
  mcp_documentation:
    description: "Project documentation and guides"
    embedding_function: "default"
  mcp_errors_solutions:
    description: "Error patterns and solutions"
    embedding_function: "default"
  mcp_github_content:
    description: "GitHub repository content"
    embedding_function: "default"
  mcp_file_content:
    description: "File system content for search"
    embedding_function: "default"
EOF
```

## Embedding Function Configuration

ChromaDB MCP supports multiple embedding providers:

### Available Embedding Functions

| Function | Provider | Use Case | Performance |
|----------|----------|----------|-------------|
| `default` | Local CPU | General purpose | Fast, balanced |
| `accurate` | Local CPU/GPU | High accuracy | Slower, accurate |
| `openai` | OpenAI API | General purpose | API cost |
| `cohere` | Cohere API | Multilingual | API cost |
| `huggingface` | HF API | Flexible models | API cost |
| `jina` | Jina API | Long context | API cost |
| `voyageai` | Voyage API | Retrieval focus | API cost |
| `gemini` | Google API | General purpose | API cost |

### Configuration Examples

```bash
# Default (local CPU) - Recommended for development
export CHROMA_EMBEDDING_FUNCTION="default"

# High accuracy (local GPU if available)
export CHROMA_EMBEDDING_FUNCTION="accurate"

# OpenAI embeddings (requires API key)
export CHROMA_EMBEDDING_FUNCTION="openai"
export OPENAI_API_KEY="your-openai-api-key"

# Sentence Transformers (specific model)
export CHROMA_EMBEDDING_FUNCTION="sentence-transformers/all-MiniLM-L6-v2"
```

## ChromaDB Integration Workflow

```mermaid
sequenceDiagram
    participant User
    participant Claude
    participant MCP as MCP Router
    participant Mem as Memory Server
    participant Chr as ChromaDB MCP
    participant CDB as ChromaDB Store
    participant TS as TimescaleDB

    User->>Claude: "Remember this conversation pattern"
    Claude->>MCP: Route to memory storage
    MCP->>Mem: Store conversation
    Mem->>Chr: Create embedding
    Chr->>CDB: Store vector + metadata
    Chr-->>Mem: Embedding ID
    Mem->>TS: Store structured data
    TS-->>Mem: Record ID
    Mem-->>MCP: Success
    MCP-->>Claude: Stored successfully
    Claude-->>User: "Pattern stored for future recall"

    Note over User,CDB: Data flows to both vector and relational storage

    User->>Claude: "Find similar conversations"
    Claude->>MCP: Route to semantic search
    MCP->>Chr: Query similar vectors
    Chr->>CDB: Semantic search
    CDB-->>Chr: Similar embeddings
    Chr->>TS: Get metadata for results
    TS-->>Chr: Full conversation data
    Chr-->>MCP: Search results
    MCP-->>Claude: Formatted results
    Claude-->>User: "Found 3 similar patterns..."
```

## Integration with Existing MCP Servers

### 1. Memory Server Integration

Update memory server to use ChromaDB for embeddings:

```bash
# Remove existing memory server
claude mcp remove memory

# Reinstall with ChromaDB integration
claude mcp add-json --scope user "memory" '{"command":"npx","args":["-y","mcp-memory-server"],"env":{"DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb","VECTOR_DATABASE":"chromadb","CHROMA_PATH":"/home/zzhang/.chromadb","CHROMA_COLLECTION":"mcp_conversations"}}'
```

### 2. Sequential Thinking with Persistent Memory

```bash
# Update sequential thinking to store in ChromaDB
claude mcp remove sequential-thinking
claude mcp add-json --scope user "sequential-thinking" '{"command":"npx","args":["-y","@modelcontextprotocol/server-sequential-thinking"],"env":{"MEMORY_BACKEND":"chromadb","CHROMA_PATH":"/home/zzhang/.chromadb","CHROMA_COLLECTION":"mcp_thinking_chains"}}'
```

### 3. GitHub Content Indexing

```bash
# Update GitHub server to index content in ChromaDB
claude mcp remove github
claude mcp add-json --scope user "github" '{"command":"npx","args":["-y","@modelcontextprotocol/server-github"],"env":{"GITHUB_PERSONAL_ACCESS_TOKEN":"your-github-token","DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb","VECTOR_DATABASE":"chromadb","CHROMA_PATH":"/home/zzhang/.chromadb","CHROMA_COLLECTION":"mcp_github_content"}}'
```

## ChromaDB Collections Management

### Initialize Collections

```python
# Create initialization script
cat > ~/.chromadb/initialize_collections.py << 'EOF'
#!/usr/bin/env python3
import chromadb
from chromadb.config import Settings
import os

# Initialize ChromaDB client
persist_directory = os.path.expanduser("~/.chromadb/data")
client = chromadb.PersistentClient(path=persist_directory)

# Define collections with metadata
collections_config = {
    "mcp_conversations": {
        "metadata": {"description": "Conversation memory and context"},
        "embedding_function": "default"
    },
    "mcp_code_snippets": {
        "metadata": {"description": "Code examples and patterns"},
        "embedding_function": "default"
    },
    "mcp_documentation": {
        "metadata": {"description": "Project documentation and guides"},
        "embedding_function": "default"
    },
    "mcp_errors_solutions": {
        "metadata": {"description": "Error patterns and solutions"},
        "embedding_function": "default"
    },
    "mcp_github_content": {
        "metadata": {"description": "GitHub repository content"},
        "embedding_function": "default"
    },
    "mcp_file_content": {
        "metadata": {"description": "File system content for search"},
        "embedding_function": "default"
    },
    "mcp_thinking_chains": {
        "metadata": {"description": "Sequential thinking patterns"},
        "embedding_function": "default"
    }
}

# Create collections
for collection_name, config in collections_config.items():
    try:
        collection = client.create_collection(
            name=collection_name,
            metadata=config["metadata"]
        )
        print(f"✓ Created collection: {collection_name}")
    except Exception as e:
        if "already exists" in str(e):
            print(f"• Collection {collection_name} already exists")
        else:
            print(f"✗ Error creating {collection_name}: {e}")

print(f"\nChromaDB initialized at: {persist_directory}")
print("Collections ready for MCP integration!")
EOF

# Make executable and run
chmod +x ~/.chromadb/initialize_collections.py
python3 ~/.chromadb/initialize_collections.py
```

## Environment Variables

Add to `~/.zshrc`:

```bash
cat >> ~/.zshrc << 'EOF'

# =============================================================================
# ChromaDB MCP Configuration
# =============================================================================

# ChromaDB Paths
export CHROMA_PATH="/home/zzhang/.chromadb"
export CHROMA_PERSIST_DIRECTORY="/home/zzhang/.chromadb/data"
export CHROMA_CONFIG_PATH="/home/zzhang/.chromadb/chroma_config.yaml"

# ChromaDB Server Configuration
export CHROMA_HOST="localhost"
export CHROMA_PORT="8000"
export CHROMA_LOG_LEVEL="INFO"

# Embedding Configuration
export CHROMA_EMBEDDING_FUNCTION="default"  # or "sentence-transformers/all-MiniLM-L6-v2"
export CHROMA_EMBEDDING_DIMENSION="384"

# Collection Settings
export CHROMA_DEFAULT_COLLECTION="mcp_conversations"
export CHROMA_AUTO_CREATE_COLLECTIONS="true"

# API Keys (if using external embedding providers)
# export OPENAI_API_KEY="your-openai-api-key"
# export COHERE_API_KEY="your-cohere-api-key"

# Performance Settings
export CHROMA_BATCH_SIZE="100"
export CHROMA_MAX_BATCH_SIZE="1000"

EOF

# Reload configuration
source ~/.zshrc
```

## Testing and Verification

### 1. Test ChromaDB Installation

```bash
# Check ChromaDB service
python3 -c "
import chromadb
client = chromadb.PersistentClient(path='~/.chromadb/data')
collections = client.list_collections()
print(f'ChromaDB collections: {[c.name for c in collections]}')
"

# Test MCP server connection
claude mcp list | grep chromadb
```

### 2. Test Embedding Functions

```python
# Test embedding creation
cat > ~/.chromadb/test_embeddings.py << 'EOF'
#!/usr/bin/env python3
import chromadb
import os

client = chromadb.PersistentClient(path=os.path.expanduser("~/.chromadb/data"))

# Get or create test collection
collection = client.get_or_create_collection("test_embeddings")

# Test document embedding
test_docs = [
    "This is a test document about ChromaDB integration",
    "MCP servers provide powerful AI tool integration",
    "Vector databases enable semantic search capabilities"
]

# Add documents
collection.add(
    documents=test_docs,
    ids=["doc1", "doc2", "doc3"],
    metadatas=[{"type": "test"} for _ in test_docs]
)

# Test similarity search
results = collection.query(
    query_texts=["vector database search"],
    n_results=2
)

print("Test documents added successfully!")
print(f"Search results: {results['documents']}")

# Clean up test collection
client.delete_collection("test_embeddings")
print("Test completed successfully!")
EOF

# Run test
python3 ~/.chromadb/test_embeddings.py
```

### 3. Integration Test with MCP

```bash
# Start Claude and test ChromaDB integration
claude

# Test commands:
# "Store this conversation in ChromaDB for future reference"
# "Search for similar code patterns in my stored memories"
# "What documentation did I save about MCP servers?"
```

## Performance and Optimization

### Storage Management

```bash
# Monitor ChromaDB storage usage
du -sh ~/.chromadb/

# Check collection sizes
python3 -c "
import chromadb
client = chromadb.PersistentClient(path='~/.chromadb/data')
for collection in client.list_collections():
    count = collection.count()
    print(f'{collection.name}: {count} documents')
"
```

### Maintenance Scripts

```bash
# Create maintenance script
cat > ~/.chromadb/maintenance.sh << 'EOF'
#!/bin/bash
set -e

echo "ChromaDB Maintenance Script"
echo "=========================="

# Backup ChromaDB data
BACKUP_DIR="$HOME/.chromadb/backups/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"
cp -r ~/.chromadb/data "$BACKUP_DIR/"
echo "✓ Backup created: $BACKUP_DIR"

# Check disk usage
echo "📊 Storage Usage:"
du -sh ~/.chromadb/

# Verify collections
echo "📚 Collections Status:"
python3 -c "
import chromadb
client = chromadb.PersistentClient(path='~/.chromadb/data')
for collection in client.list_collections():
    count = collection.count()
    print(f'  {collection.name}: {count} documents')
"

# Test basic functionality
echo "🧪 Testing basic functionality:"
python3 ~/.chromadb/test_embeddings.py > /dev/null && echo "✓ Basic operations working"

echo "Maintenance completed successfully!"
EOF

chmod +x ~/.chromadb/maintenance.sh
```

## Troubleshooting

### Common Issues

1. **Permission Errors**
   ```bash
   # Fix ownership
   sudo chown -R $(whoami):$(whoami) ~/.chromadb/
   chmod -R 755 ~/.chromadb/
   ```

2. **Port Conflicts**
   ```bash
   # Check if port 8000 is in use
   lsof -i :8000
   # Use different port if needed
   export CHROMA_PORT="8001"
   ```

3. **Embedding Function Errors**
   ```bash
   # Reset to default embedding function
   export CHROMA_EMBEDDING_FUNCTION="default"
   ```

4. **Collection Corruption**
   ```bash
   # Reset specific collection
   python3 -c "
   import chromadb
   client = chromadb.PersistentClient(path='~/.chromadb/data')
   client.delete_collection('collection_name')
   "
   ```

### Recovery Procedures

```bash
# Complete ChromaDB reset (if needed)
rm -rf ~/.chromadb/data/*
python3 ~/.chromadb/initialize_collections.py

# Restart MCP servers
claude mcp remove chromadb
# Re-add ChromaDB MCP server
claude mcp add-json --scope user "chromadb" '{"command":"npx","args":["-y","chroma-mcp-server"],"env":{"CHROMA_PATH":"/home/zzhang/.chromadb","CHROMA_EMBEDDING_FUNCTION":"default","CHROMA_PERSIST_DIRECTORY":"/home/zzhang/.chromadb/data"}}'
```

## Integration with Other Vector Databases

### ChromaDB vs Qdrant Usage

| Use Case | Recommended Database | Reason |
|----------|---------------------|---------|
| Development/Testing | ChromaDB | Local, no setup required |
| Production Scale | Qdrant | High performance, distributed |
| Conversation Memory | ChromaDB | Persistent, local storage |
| Large Vector Sets | Qdrant | Optimized for scale |
| Multi-tenant | Qdrant | Better isolation |
| Simple Embeddings | ChromaDB | Easier setup and maintenance |

### Hybrid Configuration

```bash
# Use both ChromaDB and Qdrant
# ChromaDB for persistent memory and conversations
# Qdrant for high-performance vector operations

# This is already configured in your setup:
# - ChromaDB: Local persistent memory
# - Qdrant: High-performance vector search
# - TimescaleDB: Structured data and time series
```

This setup provides comprehensive vector database capabilities with ChromaDB handling persistent memory and local embeddings while Qdrant manages high-performance vector operations.