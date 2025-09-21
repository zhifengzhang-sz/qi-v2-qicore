# Smithery Installation Workflow

## What Just Happened

Smithery is indeed the better method! Here's what the installation process involves:

## Pre-Installation Setup

### 1. Get Smithery API Key
- Visit: https://smithery.ai/account/api-keys
- Create a free account
- Generate an API key
- Save it for the installation process

### 2. Prepare Paths
Smithery will ask for these paths during installation:
- **Backup Path**: `./.smithery` (recommended)
- **ChromaDB Path**: `./.chromadb` (recommended)

## Installation Process

### 1. Run Smithery Command
```bash
npx @smithery/cli install @doobidoo/mcp-memory-service --client claude
```

### 2. Interactive Prompts
The installer will ask:

1. **Package Installation Consent**
   ```
   Need to install the following packages:
   @smithery/cli@1.3.0
   Ok to proceed? (y)
   ```
   Answer: `y`

2. **Usage Analytics**
   ```
   Would you like to help improve Smithery by sending anonymized usage data?
   ```
   Answer: `Yes` or `No` (your choice)

3. **API Key**
   ```
   Please enter your Smithery API key (get one for free from https://smithery.ai/account/api-keys):
   ```
   Paste your API key here

4. **Configuration Paths**
   ```
   Path for backups. (required)
   ```
   Suggest: `./.smithery`

   ```
   Path to ChromaDB storage. (required)
   ```
   Suggest: `./.chromadb`

5. **Claude App Restart**
   ```
   Would you like to restart the claude app to apply changes?
   ```
   Answer: `Yes`

### 3. Post-Installation
- Smithery automatically restarts Claude
- Your terminal session ends (this is normal)
- MCP server is configured and ready

## Streamlined Installation Commands

### One-Time Setup (First Installation)
```bash
# 1. Get API key from https://smithery.ai/account/api-keys
# 2. Create directories
mkdir -p ./.smithery ./.chromadb

# 3. Install first server (interactive setup)
npx @smithery/cli install @doobidoo/mcp-memory-service --client claude
# Follow prompts above
```

### Subsequent Installations (Non-Interactive)
After the first setup, you can use:

```bash
# Memory service (already done if you followed above)
npx @smithery/cli install @doobidoo/mcp-memory-service --client claude

# Sequential thinking
npx @smithery/cli install sequential-thinking --client claude

# Context7
npx @smithery/cli install @upstash/context7-mcp --client claude

# Note: Some servers may not be available via Smithery
```

## What Smithery Does

1. **Downloads and configures** the MCP server
2. **Sets up proper paths** for data storage
3. **Adds configuration** to Claude's MCP settings
4. **Handles dependencies** automatically
5. **Restarts Claude** to apply changes

## Smithery vs Manual Installation

| Method | Pros | Cons |
|--------|------|------|
| Smithery | ✅ Automatic setup<br>✅ Handles dependencies<br>✅ Pre-configured paths<br>✅ Restart handling | ❌ Requires API key<br>❌ Limited server selection<br>❌ Exits terminal |
| Manual | ✅ Full control<br>✅ All servers available<br>✅ No external dependencies | ❌ Manual configuration<br>❌ Path setup required<br>❌ More complex |

## Recommended Hybrid Approach

1. **Use Smithery for supported servers:**
   - `@doobidoo/mcp-memory-service`
   - `sequential-thinking`
   - `@upstash/context7-mcp`

2. **Use manual installation for others:**
   - `mcp-server-qdrant` (requires uvx)
   - `@brave/brave-search-mcp-server` (needs API key)

## Verification After Smithery Installation

```bash
# Start new Claude session
claude

# Check installed servers
claude mcp list

# Test the installed server
claude mcp get mcp-memory-service
```

## Troubleshooting Smithery

### API Key Issues
```bash
# If API key prompt appears again
# Check if key was saved properly
cat ~/.smithery/config.json

# Re-enter key manually
npx @smithery/cli login
```

### Path Issues
```bash
# If paths aren't created
mkdir -p ./.smithery ./.chromadb
chmod 755 ./.smithery ./.chromadb
```

### Claude Restart Issues
```bash
# If Claude doesn't restart automatically
pkill -f claude
claude
```

## Smithery Commands Reference

```bash
# List installed servers
npx @smithery/cli list --client claude

# Search available servers
npx @smithery/cli search memory

# Install server
npx @smithery/cli install <server> --client claude

# Uninstall server
npx @smithery/cli uninstall <server> --client claude

# Login with API key
npx @smithery/cli login

# Show help
npx @smithery/cli --help
```

## Next Steps After Memory Service Installation

1. **Install remaining servers** using Smithery where possible
2. **Manual installation** for unsupported servers (Qdrant, Brave Search)
3. **Test all servers** individually
4. **Configure environment variables** for manual servers

The memory service installation via Smithery was successful! The terminal exit and Claude restart are normal parts of the process.