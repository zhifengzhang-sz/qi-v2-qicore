```bash
#!/bin/bash

# ============================================
# STEP 3: Run the Commands (Simple Usage)
# ============================================

# Navigate to your project
cd ~/dev/qi/github/qi-v2-qicore

# ============================================
# Individual Commands
# ============================================

# Generate design documentation
claude /design-docs

# Generate code from design
claude /generate-code

# Fix all stubs and placeholders
claude /fix-stubs

# Fix TypeScript type errors
claude /typecheck-fix

# Fix linting and formatting
claude /biome-fix

# Generate comprehensive tests
claude /generate-tests

# Quick quality check
claude /quick-check

# ============================================
# Complete Workflow (One Command)
# ============================================

# Run the entire development workflow
claude /dev-workflow

# ============================================
# Real Usage Examples
# ============================================

# Starting a new feature
cd ~/dev/qi/github/qi-v2-qicore
claude /design-docs
claude /generate-code
claude /generate-tests

# Code quality cleanup
claude /fix-stubs
claude /typecheck-fix
claude /biome-fix

# Before committing
claude /quick-check

# Complete development cycle
claude /dev-workflow

# ============================================
# Project-Specific Usage
# ============================================

# For qi-v2-qicore
cd ~/dev/qi/github/qi-v2-qicore
claude /dev-workflow

# For qi-v2-dp
cd ~/dev/qi/github/qi-v2-dp
claude /dev-workflow

# For qi-v2-llm
cd ~/dev/qi/github/qi-v2-llm
claude /dev-workflow

# ============================================
# Custom Variations
# ============================================

# Just documentation and code generation
claude /design-docs
claude /generate-code

# Just quality checks
claude /typecheck-fix
claude /biome-fix
claude /quick-check

# Just testing
claude /generate-tests
npm test

# ============================================
# Integration with Git
# ============================================

# Before committing
git add .
claude /quick-check
# Only commit if quick-check passes
git commit -m "feat: implement new feature"

# Before pushing
claude /dev-workflow
git push

# ============================================
# Package.json Scripts (Optional)
# ============================================

# Add to your package.json for convenience
cat >> package.json << 'EOF'
{
  "scripts": {
    "dev:docs": "claude /design-docs",
    "dev:code": "claude /generate-code",
    "dev:stubs": "claude /fix-stubs",
    "dev:types": "claude /typecheck-fix",
    "dev:lint": "claude /biome-fix",
    "dev:test": "claude /generate-tests",
    "dev:check": "claude /quick-check",
    "dev:workflow": "claude /dev-workflow"
  }
}
EOF

# Now you can use npm scripts too
npm run dev:workflow
npm run dev:check

# ============================================
# Real-World Example: Adding Authentication
# ============================================

cd ~/dev/qi/github/qi-v2-qicore

# 1. Update design docs for new auth system
claude "I'm adding user authentication with JWT tokens. Please update the design documentation to include the auth system architecture, API endpoints, and data models."

# 2. Generate the authentication code
claude /generate-code

# 3. Fix any stubs in the auth implementation
claude /fix-stubs

# 4. Ensure type safety
claude /typecheck-fix

# 5. Generate tests for auth system
claude /generate-tests

# 6. Final quality check
claude /quick-check

# Done! Authentication system is implemented with docs, tests, and quality checks.

# ============================================
# Debugging and Troubleshooting
# ============================================

# If a command fails, you can debug step by step:

# Check what MCP servers are available
claude mcp list

# Run a specific check
claude "Run TypeScript compiler and show me any errors"

# Check file system access
claude "List all files in the src directory"

# Check git status
claude "Show me the current git status and recent commits"

# ============================================
# Advanced Usage
# ============================================

# Combine with other Claude Code features
claude "Analyze the performance of qi-v2-qicore and then run /dev-workflow to optimize it"

claude "Review the security of qi-v2-qicore auth system and then run /generate-tests to add security tests"

claude "Compare qi-v2-qicore with qi-v2-dp architecture and then run /design-docs to document the differences"

echo "🎯 That's it! Your simple workflow is ready to use."
echo ""
echo "Summary of what you have:"
echo "• MCP servers for file/git/github operations"
echo "• Custom commands for each workflow step"
echo "• Simple /command syntax to run them"
echo ""
echo "Usage:"
echo "• cd to your project directory"
echo "• claude /dev-workflow (complete workflow)"
echo "• claude /quick-check (fast quality check)"
echo "• Individual commands as needed"
echo ""
echo "No complex orchestration, no frameworks, just simple commands! 🚀"
```