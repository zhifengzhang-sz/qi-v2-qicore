# Jupyter Notebook Setup for QiCore Development

## Overview

This guide covers setting up Jupyter Notebook with TypeScript support for creating interactive QiCore tutorials and development.

## Prerequisites

- Python 3.13+ with pip installed via pyenv
- VSCode installed
- QiCore TypeScript packages built

## Installation

### 1. Python Environment Setup

```bash
# Ensure Python 3.13.7 is active
pyenv global 3.13.7
python --version  # Should show Python 3.13.7
pip --version     # Should show pip 25.2+
```

### 2. Speed Up pip Downloads (Optional but Recommended)

For faster package downloads, configure a mirror:

```bash
# Create pip config directory
mkdir -p ~/.config/pip

# Add fast mirror configuration
cat > ~/.config/pip/pip.conf << 'EOF'
[global]
index-url = https://pypi.tuna.tsinghua.edu.cn/simple
trusted-host = pypi.tuna.tsinghua.edu.cn
timeout = 120
retries = 5
EOF
```

### 3. Install Jupyter

```bash
# Install Jupyter Notebook and JupyterLab
pip install jupyter jupyterlab

# Verify installation
jupyter --version
```

### 4. Install Deno for TypeScript Kernel

```bash
# Install Deno
curl -fsSL https://deno.land/install.sh | sh

# Add Deno to PATH (add to ~/.bashrc or ~/.zshrc)
echo 'export PATH="$HOME/.deno/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc

# Verify Deno installation
deno --version
```

### 5. Install Deno Jupyter Kernel

```bash
# Install the Deno kernel for Jupyter
deno jupyter --install

# Verify kernel installation
jupyter kernelspec list
```

Expected output should include `deno` kernel.

## VSCode Integration

### 1. Install Required Extensions

```bash
# Install Jupyter extensions for VSCode
code --install-extension ms-toolsai.jupyter
code --install-extension ms-toolsai.jupyter-keymap  
code --install-extension ms-toolsai.jupyter-renderers
code --install-extension denoland.vscode-deno
```

### 2. Configure VSCode Settings

Add to your VSCode `settings.json` (Ctrl+Shift+P → "Open Settings JSON"):

```json
{
  "jupyter.defaultKernel": "deno",
  "deno.enable": true,
  "deno.unstable": true,
  "jupyter.interactiveWindow.textEditor.executeSelection": true,
  "notebook.defaultRenderer": "jupyter-notebook",
  "deno.path": "/home/zzhang/.deno/bin/deno"
}
```

### 3. Workspace-Specific Settings

For the QiCore project, create `.vscode/settings.json`:

```json
{
  "deno.enable": true,
  "deno.unstable": true,
  "deno.importMap": "./import_map.json",
  "jupyter.notebookFileRoot": "${workspaceFolder}/typescript/tutorials"
}
```

## Usage

### 1. Start Jupyter in VSCode

1. Open VSCode in the QiCore project
2. Create a new file with `.ipynb` extension
3. VSCode will prompt to select a kernel - choose "Deno"
4. Start writing TypeScript code in cells

### 2. Start Jupyter in Browser

```bash
# From project root
cd typescript

# Start Jupyter Lab
jupyter lab

# Or classic Jupyter Notebook
jupyter notebook
```

### 3. Create QiCore Tutorial Notebook

Example first cell for QiCore tutorial:

```typescript
// Import QiCore packages
import { type Result, success, failure, match } from '@qi/base';
import { createLogger } from '@qi/core';

console.log('🚀 QiCore Tutorial Started');

// Test Result<T> pattern
const divide = (a: number, b: number): Result<number, string> => 
  b === 0 ? failure("Division by zero") : success(a / b);

const result = divide(10, 2);
match(
  (value) => console.log(`✅ Success: ${value}`),
  (error) => console.error(`❌ Error: ${error}`),
  result
);
```

## Directory Structure

Create this structure for tutorials:

```
typescript/
├── tutorials/
│   ├── notebooks/                 # Jupyter notebooks
│   │   ├── 01-result-basics.ipynb
│   │   ├── 02-error-handling.ipynb
│   │   ├── 03-async-patterns.ipynb
│   │   └── 04-infrastructure.ipynb
│   └── executable/                # Regular TypeScript tutorials
│       ├── 01-result-basics.ts
│       └── 02-error-handling.ts
└── docs/
    └── tutorials/                 # Tutorial documentation
```

## Troubleshooting

### Common Issues

1. **Kernel not found**: Ensure Deno is in PATH and kernel is installed
   ```bash
   which deno
   deno jupyter --install
   ```

2. **Import errors**: QiCore packages need to be built first
   ```bash
   cd typescript
   bun run build
   ```

3. **Permission errors**: Ensure pyenv Python is active
   ```bash
   pyenv which python
   pyenv which pip
   ```

### Performance Tips

1. **Use local mirrors** for faster pip downloads (see step 2 above)
2. **Restart VSCode** after installing extensions
3. **Use JupyterLab** for better performance with large notebooks

## Features Available

### With Deno Kernel

- ✅ **Native TypeScript support** - No transpilation needed
- ✅ **Top-level await** - Async code in any cell
- ✅ **Import from URLs** - Direct npm and HTTP imports
- ✅ **Built-in testing** - Deno.test() available
- ✅ **Rich outputs** - HTML, markdown, charts via Deno.jupyter

### Example Rich Output

```typescript
// Display HTML output
Deno.jupyter.html`
<div style="color: blue; font-size: 18px;">
  <h3>QiCore Result Pattern</h3>
  <p>This demonstrates functional error handling</p>
</div>
`;

// Display markdown
Deno.jupyter.md`
## Result<T> Success
- ✅ Type-safe error handling
- ✅ Composable operations  
- ✅ No exceptions thrown
`;
```

## Next Steps

1. Create tutorial notebooks in `typescript/tutorials/notebooks/`
2. Start with basic Result<T> patterns
3. Progress to infrastructure services (logger, cache, config)
4. Add interactive examples for CLI and AMSG integration

The interactive nature of Jupyter makes it perfect for teaching QiCore's functional programming patterns!