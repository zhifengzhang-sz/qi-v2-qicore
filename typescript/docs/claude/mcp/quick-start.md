# MCP Quick Start Guide

## TL;DR Installation Commands

Run these commands in order:

```bash
# 1. Install core tools
curl -LsSf https://astral.sh/uv/install.sh | sh

# 2. CRITICAL: Reload shell to get UV in PATH
source ~/.zshrc
# (or source ~/.bashrc for Bash users)

# 3. Verify tools work
uv --version && uvx --version

# 4. Install ALL MCP servers using Claude Code native commands (WORKING METHOD)
claude mcp add-json --scope user "sequential-thinking" '{"command":"npx","args":["-y","@modelcontextprotocol/server-sequential-thinking"]}'

claude mcp add-json --scope user "memory" '{"command":"npx","args":["-y","mcp-memory-server"],"env":{"DATABASE_URL":"postgresql://postgres:password@localhost:5432/cryptodb"}}'

claude mcp add-json --scope user "qdrant" '{"command":"uvx","args":["mcp-server-qdrant"],"env":{"QDRANT_URL":"http://localhost:6333","COLLECTION_NAME":"mcp-memories","EMBEDDING_MODEL":"sentence-transformers/all-MiniLM-L6-v2"}}'

claude mcp add-json --scope user "brave-search" '{"command":"npx","args":["-y","@brave/brave-search-mcp-server"],"env":{"BRAVE_API_KEY":"YOUR_API_KEY"}}'

claude mcp add-json --scope user "context7" '{"command":"npx","args":["-y","@upstash/context7-mcp@latest"]}'

# 5. Verify installation (should show all 5 connected)
claude mcp list
```

## Environment Variables Needed

```bash
export DATABASE_URL="postgresql://postgres:password@localhost:5432/cryptodb"
export QDRANT_URL="http://localhost:6333"
export BRAVE_API_KEY="your-brave-api-key"
```

## Quick Test Commands

```bash
# Test databases
curl http://localhost:6333/health
psql $DATABASE_URL -c "SELECT 1;"

# Test MCP servers
claude mcp test sequential-thinking
claude mcp test memory
claude mcp test qdrant
claude mcp test brave-search
claude mcp test context7
```

## Common Issues & Quick Fixes

| Issue | Quick Fix |
|-------|-----------|
| `uv not found` | `curl -LsSf https://astral.sh/uv/install.sh \| sh && source ~/.bashrc` |
| NPX timeout | `npm config set timeout 300000` |
| Python deps fail | `pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cpu` |
| Database connection | `docker restart qicore-timescaledb qicore-qdrant` |
| Smithery fails | Use manual `claude mcp add-json` commands |

## One-Line Status Check

```bash
echo "Tools:"; which uv uvx npm node python3; echo "Databases:"; curl -s http://localhost:6333/health && psql $DATABASE_URL -c "SELECT 1;" 2>/dev/null && echo "✓ All DB OK"; echo "MCP:"; claude mcp list
```