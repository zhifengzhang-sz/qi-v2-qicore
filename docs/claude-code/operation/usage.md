
## The Simple Way: 3 Steps
Steps 1 & 2: [setup.sh](./step.1-2.md), only need to run once.

## STEP 3: Run the Commands
[run.sh](./step.3.md)

## That's It! Super Simple! 🎯

**Your entire software development workflow is now:**

```bash
# One-time setup (5 minutes)
./setup.sh

# Daily usage (30 seconds)
cd qi-v2-qicore
claude /dev-workflow

# Or step by step
claude /design-docs
claude /generate-code
claude /fix-stubs
claude /typecheck-fix
claude /biome-fix
claude /generate-tests
claude /quick-check
```

## **Why This is Better Than Complex Solutions**

✅ **No frameworks** - just MCP servers + custom commands  
✅ **No orchestration** - Claude Code handles the logic  
✅ **No complex setup** - works out of the box  
✅ **Easy to modify** - just edit the .md files  
✅ **Easy to debug** - run individual commands  
✅ **Reusable** - works across all your qi projects  

## **What You Get**

- **Design docs** automatically generated from your code
- **Type-safe code** generation from design docs
- **Stub detection** and automatic fixes
- **TypeScript compliance** checking and fixes
- **Code formatting** and linting
- **Comprehensive tests** generated automatically
- **Quality gates** before commits

## **Real Usage Flow**

```bash
# Morning: Start new feature
cd qi-v2-qicore
claude /design-docs
claude /generate-code

# Afternoon: Code quality pass
claude /fix-stubs
claude /typecheck-fix
claude /biome-fix

# Evening: Before commit
claude /generate-tests
claude /quick-check
git commit -m "feat: new auth system"
```

**No sky-walking between languages, no complex architectures, just simple commands that work!** 🚀

Want to try it? Just run the setup script and you're good to go!