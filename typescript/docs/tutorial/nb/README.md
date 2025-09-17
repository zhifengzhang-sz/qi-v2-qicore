# QiCore Jupyter Notebook Tutorials

**Interactive learning for the QiCore Foundation ecosystem**

## 🎯 Overview

This directory contains comprehensive Jupyter notebook tutorials that guide you through all QiCore modules with hands-on, executable examples. Each notebook is designed to be run in sequence, building upon concepts from previous modules.

## 📚 Tutorial Sequence

### [01-qi-base.ipynb](./01-qi-base.ipynb) - Foundation Patterns
**Duration: ~30-45 minutes**

- ✅ Result<T> fundamentals and type-safe error handling
- ✅ Error categories and retry strategies
- ✅ Functional composition with map, flatMap, match
- ✅ Async patterns and Promise<Result<T>> handling
- ✅ Real-world user registration example

**Prerequisites**: Basic TypeScript knowledge  
**Outputs**: Understanding of functional error handling patterns

### [02-qi-core.ipynb](./02-qi-core.ipynb) - Infrastructure Services
**Duration: ~45-60 minutes**

- ✅ Configuration management from multiple sources
- ✅ Structured logging with context accumulation  
- ✅ Unified caching (Memory and Redis backends)
- ✅ Service integration patterns
- ✅ Production e-commerce service example

**Prerequisites**: [01-qi-base.ipynb](./01-qi-base.ipynb)  
**Outputs**: Ability to build robust production services

### [03-qi-amsg.ipynb](./03-qi-amsg.ipynb) - Message-Driven Architecture
**Duration: ~45-60 minutes**

- ✅ Message type system and priority queues
- ✅ h2A-inspired async iteration patterns
- ✅ CLI-Agent integration via message queues
- ✅ Streaming data and backpressure handling
- ✅ Complete message-driven application example

**Prerequisites**: [01-qi-base.ipynb](./01-qi-base.ipynb), [02-qi-core.ipynb](./02-qi-core.ipynb)  
**Outputs**: Understanding of scalable message-driven systems

### [04-qi-cli.ipynb](./04-qi-cli.ipynb) - CLI Framework
**Duration: ~60-75 minutes**

- ✅ XState-powered hierarchical state management
- ✅ Command system (CLI vs Agent separation)
- ✅ Message-driven CLI ↔ Agent communication
- ✅ UI framework adapters (Readline, Hybrid)
- ✅ Complete CLI application with all integrations

**Prerequisites**: All previous notebooks  
**Outputs**: Ability to build sophisticated CLI applications

## 🚀 Getting Started

### Prerequisites

1. **Jupyter Setup**: Follow the [Jupyter setup guide](../../devop/windows-11-pro/setup.nb.md)
2. **QiCore Build**: Ensure QiCore packages are built (`bun run build`)
3. **Deno Kernel**: Install Deno Jupyter kernel for TypeScript support

### Quick Start

```bash
# 1. Navigate to the tutorial directory
cd typescript/docs/tutorial/nb

# 2. Start Jupyter Lab
jupyter lab

# 3. Open notebooks in sequence and execute cells
```

### VSCode Integration

```bash
# 1. Install Jupyter extensions
code --install-extension ms-toolsai.jupyter

# 2. Open the notebook directory in VSCode
code .

# 3. Open .ipynb files and select Deno kernel
```

## 📋 Learning Path

### Beginner Path (New to Functional Programming)
1. Start with [01-qi-base.ipynb](./01-qi-base.ipynb) - Take your time with Result<T> concepts
2. Practice with the working examples in `typescript/app/basic-result/`
3. Move to [02-qi-core.ipynb](./02-qi-core.ipynb) when comfortable with error handling
4. Continue sequentially through remaining notebooks

### Experienced Path (Familiar with FP)
1. Quick review of [01-qi-base.ipynb](./01-qi-base.ipynb) for QiCore-specific patterns
2. Focus on integration patterns in [02-qi-core.ipynb](./02-qi-core.ipynb)
3. Deep dive into message architecture in [03-qi-amsg.ipynb](./03-qi-amsg.ipynb)
4. Explore advanced CLI patterns in [04-qi-cli.ipynb](./04-qi-cli.ipynb)

### Project-Focused Path (Building Something Specific)
- **Web API**: Focus on [01-qi-base.ipynb](./01-qi-base.ipynb) + [02-qi-core.ipynb](./02-qi-core.ipynb)
- **CLI Tool**: Complete all notebooks, emphasizing [04-qi-cli.ipynb](./04-qi-cli.ipynb)
- **Microservices**: Emphasize [03-qi-amsg.ipynb](./03-qi-amsg.ipynb) for service communication
- **Data Processing**: Focus on [01-qi-base.ipynb](./01-qi-base.ipynb) + [03-qi-amsg.ipynb](./03-qi-amsg.ipynb)

## 🎯 Learning Objectives

By completing these tutorials, you will:

### Core Skills
- Master functional error handling with Result<T>
- Build reliable services with proper configuration and logging
- Design scalable message-driven architectures
- Create sophisticated CLI applications with state management

### Advanced Skills
- Apply functional programming patterns to TypeScript
- Implement production-ready error categorization and retry logic
- Design async communication patterns with priority handling
- Build pluggable UI frameworks with component architecture

### Production Skills
- Write maintainable code that handles errors explicitly
- Build observable systems with structured logging
- Design responsive systems with proper backpressure handling
- Create user-friendly CLI tools with rich interaction patterns

## 🛠️ Troubleshooting

### Common Issues

**Import Errors**
```typescript
// If you see: Cannot find module '@qi/base'
// Solution: Ensure packages are built and imports are correct
import { success } from '@qi/base'  // ✅ Correct
```

**Kernel Issues**
```bash
# If Deno kernel not available
deno jupyter --install
jupyter kernelspec list  # Should show 'deno'
```

**Execution Errors**
```bash
# If cells fail to execute
cd typescript
bun run build  # Rebuild packages
bun run check  # Verify everything works
```

### Performance Tips

1. **Run notebooks sequentially** - Each builds on the previous
2. **Execute all cells in order** - Later cells depend on earlier setup
3. **Restart kernel if needed** - If imports break, restart and re-run
4. **Use VSCode for debugging** - Better error messages and IntelliSense

## 📚 Additional Resources

### Working Examples
- `typescript/app/basic-result/` - Simple Result<T> examples
- `typescript/app/config-example/` - Configuration and logging integration
- `typescript/app/cli-amsg-example/` - Complete CLI-AMSG integration

### Documentation
- [API Documentation](../api/README.md) - Complete API reference
- [Architecture Guides](../../README.md) - System design documentation
- [Setup Guide](../setup.md) - Project configuration

### External Resources
- [Deno Jupyter Documentation](https://deno.land/manual/tools/jupyter)
- [XState Documentation](https://xstate.js.org/docs/) - State machine patterns
- [Functional Programming in TypeScript](https://gcanti.github.io/fp-ts/) - FP concepts

## 🤝 Contributing

Found an issue or want to improve the tutorials?

1. **Report Issues**: Create issues in the main repository
2. **Suggest Improvements**: Open PRs with tutorial enhancements
3. **Add Examples**: Contribute additional real-world examples
4. **Update Documentation**: Help keep tutorials current with code changes

## 📄 License

These tutorials are part of the QiCore project and follow the same licensing terms.

---

**Happy Learning! 🎉**

Start your QiCore journey with [01-qi-base.ipynb](./01-qi-base.ipynb) and build amazing TypeScript applications!