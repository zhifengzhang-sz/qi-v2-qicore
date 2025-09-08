# Windows 11 + WSL2: ML/AI Project Structure and File Access Guide

## Understanding WSL2 Bidirectional File Access

### **The Two-Way Access Pattern**

WSL2 provides asymmetric but complementary file access between Windows and Ubuntu:

**Ubuntu → Windows Access:**
```bash
# Ubuntu accessing Windows C: drive
/mnt/c/Users/username/Documents/
/mnt/c/dev/
/mnt/c/Program Files/

# Other Windows drives
/mnt/d/  # D: drive
/mnt/e/  # E: drive
```

**Windows → Ubuntu Access:**
```
# Windows accessing Ubuntu filesystem
\\wsl.localhost\Ubuntu-24.04\home\username\
\\wsl.localhost\Ubuntu-24.04\etc\
\\wsl.localhost\Ubuntu-24.04\var\

# Alternative syntax (also works)
\\wsl$\Ubuntu-24.04\home\username\
```

### **Why This Pattern Exists**

**Linux Philosophy:**
- **"Everything is a file"** - Windows drives are "mounted" as directories
- **Mount points** - Windows drives appear under `/mnt/` directory
- **Unified namespace** - All storage accessible through single filesystem tree

**Windows Philosophy:**
- **Network-style access** - Ubuntu appears as a "network location"
- **UNC paths** - Universal Naming Convention (`\\server\share` format)
- **Drive letters** remain separate from Linux filesystem

### **Visual Representation**

```
Ubuntu Filesystem Tree:
/
├── home/
│   └── username/          ← \\wsl.localhost\Ubuntu-24.04\home\username\
├── mnt/
│   ├── c/                 ← Windows C: drive
│   ├── d/                 ← Windows D: drive
│   └── wsl/               ← WSL-specific mounts
├── etc/
├── var/
└── usr/

Windows View:
C:\                        ← /mnt/c/ from Ubuntu
├── Users\
├── Program Files\
└── dev\

Network Locations:
\\wsl.localhost\
└── Ubuntu-24.04\         ← Ubuntu's entire filesystem
    ├── home\
    ├── etc\
    └── var\
```

### **Performance Implications**

| Direction | Path | Performance | Use Case |
|-----------|------|-------------|----------|
| **Ubuntu → Ubuntu** | `~/projects/` | ⚡ **Fastest** | Development work |
| **Windows → Ubuntu** | `\\wsl.localhost\Ubuntu-24.04\home\user\` | ⚡ **Fast** | Windows apps editing Ubuntu files |
| **Ubuntu → Windows** | `/mnt/c/Users/user/` | 🐌 **Slower** | Accessing Windows documents |
| **Windows → Windows** | `C:\projects\` | ⚡ **Fast** | Windows native |

**Why Different Performance:**
- **Native filesystem** operations are fastest
- **Cross-platform access** has translation overhead
- **WSL2 VM boundary** adds latency for `/mnt/c/` access

## Modern ML/AI Project Structure for Windows 11

### **Recommended Project Location**

**Use Ubuntu Native Filesystem (Optimal Performance):**
```bash
# Create projects in Ubuntu's home directory
mkdir -p ~/dev/ai-projects/my-project
cd ~/dev/ai-projects/my-project
```

**Windows Access Path:**
```
\\wsl.localhost\Ubuntu-24.04\home\username\dev\ai-projects\my-project
```

### **Why Ubuntu Native Filesystem is Better:**
- ⚡ **Faster I/O performance** for compilation and file operations
- 🔧 **Better Linux tool compatibility** 
- 📁 **Seamless Windows 11 integration** via `\\wsl.localhost\`

### **Complete Project Architecture**

```bash
# Example structure:
/home/username/dev/ai-projects/my-ai-app/
├── python/                    # 🐍 ML computation & training
│   ├── src/
│   │   ├── models/           # Model definitions
│   │   ├── training/         # Training scripts
│   │   ├── inference/        # Inference engines
│   │   └── utils/            # Utility functions
│   ├── notebooks/            # Jupyter notebooks
│   ├── tests/               # Python tests
│   ├── requirements.txt     # Python dependencies
│   ├── pyproject.toml       # Poetry configuration
│   └── README.md
├── typescript/               # 🟦 LLM APIs & web interfaces
│   ├── src/
│   │   ├── api/             # REST API routes
│   │   ├── services/        # Business logic
│   │   ├── integrations/    # LLM API clients
│   │   └── utils/           # Utility functions
│   ├── web/                 # Frontend applications
│   ├── tests/               # TypeScript tests
│   ├── package.json         # Node.js dependencies
│   └── README.md
├── shared/                   # 📁 Cross-platform resources
│   ├── data/                # Datasets
│   ├── models/              # Trained model files (.pt, .onnx)
│   ├── configs/             # Configuration files
│   └── api-contracts/       # Type definitions & schemas
├── docs/                     # 📚 Documentation
│   ├── architecture.md
│   ├── api-docs/
│   └── deployment.md
├── .vscode/                  # VSCode configuration
│   ├── settings.json
│   ├── launch.json
│   └── extensions.json
├── docker-compose.yml        # Development environment
├── .gitignore
└── README.md
```

**Windows 11 Access:**
```
\\wsl.localhost\Ubuntu-24.04\home\username\dev\ai-projects\my-ai-app\
```

## Project Creation Script

```bash
# Modern Windows 11 + WSL2 project setup
create_ai_project_modern() {
    local project_name=$1
    local base_dir="$HOME/dev/ai-projects/$project_name"
    local windows_path="\\\\wsl.localhost\\Ubuntu-24.04\\home\\$(whoami)\\dev\\ai-projects\\$project_name"
    
    echo "Creating AI/ML project: $project_name"
    echo "Ubuntu location: $base_dir"
    echo "Windows access: $windows_path"
    
    # Create directory structure
    mkdir -p "$base_dir"/{python/{src/{models,training,inference,utils},notebooks,tests},typescript/{src/{api,services,integrations,utils},web,tests},shared/{data,models,configs,api-contracts},docs/{api-docs},.vscode}
    
    # Initialize Python environment
    cd "$base_dir/python"
    python -m venv venv
    source venv/bin/activate
    pip install --upgrade pip
    cat > requirements.txt << EOF
torch>=2.0.0
fastapi>=0.100.0
uvicorn>=0.20.0
jupyter>=1.0.0
pandas>=2.0.0
numpy>=1.24.0
scikit-learn>=1.3.0
EOF
    pip install -r requirements.txt
    
    # Initialize TypeScript environment
    cd "$base_dir/typescript"
    npm init -y
    npm install -D typescript @types/node ts-node nodemon prettier
    npm install express @anthropic-ai/sdk openai cors dotenv
    
    # Create basic configuration files
    cat > "$base_dir/.gitignore" << EOF
# Dependencies
node_modules/
venv/
__pycache__/
*.pyc
*.pyo

# Environment and secrets
.env
.env.local
*.log

# Model files (too large for git)
shared/models/*.pt
shared/models/*.onnx
shared/data/*.csv
shared/data/*.json

# IDE
.vscode/settings.local.json
*.swp
*.swo

# OS
.DS_Store
Thumbs.db
EOF
    
    # Create README
    cat > "$base_dir/README.md" << EOF
# $project_name

ML/AI project with Python backend and TypeScript frontend.

## Structure
- \`python/\` - ML computation, training, and inference
- \`typescript/\` - APIs, web interfaces, and LLM integrations  
- \`shared/\` - Models, data, and configuration files
- \`docs/\` - Documentation

## Setup
\`\`\`bash
# Python environment
cd python && source venv/bin/activate && pip install -r requirements.txt

# TypeScript environment  
cd typescript && npm install
\`\`\`

## Access from Windows
\`\`\`
$windows_path
\`\`\`
EOF
    
    # Initialize git
    cd "$base_dir"
    git init
    git add .
    git commit -m "Initial project structure"
    
    echo ""
    echo "✅ Project '$project_name' created successfully!"
    echo "📁 Ubuntu path: $base_dir"
    echo "🪟 Windows path: $windows_path" 
    echo ""
    echo "Next steps:"
    echo "1. Open in VSCode: code $base_dir"
    echo "2. Or from Windows: Open File Explorer -> $windows_path"
}

# Usage example:
# create_ai_project_modern "my-ml-app"
```

## VSCode Integration

### **Opening Projects from Windows**

**Method 1: File Explorer**
1. Open File Explorer
2. Navigate to: `\\wsl.localhost\Ubuntu-24.04\home\username\dev\ai-projects\`
3. Right-click project folder → "Open with Code"

**Method 2: VSCode Terminal**
```bash
# From WSL terminal
cd ~/dev/ai-projects/my-ai-app
code .
```

**Method 3: Windows Quick Access**
1. Pin `\\wsl.localhost\Ubuntu-24.04\home\username\dev\` to Quick Access
2. Navigate to projects easily

### **Updated .vscode/settings.json for Ubuntu Native Filesystem**

```json
{
  "python.defaultInterpreterPath": "./python/venv/bin/python",
  "python.terminal.activateEnvironment": true,
  "python.formatting.provider": "black",
  "python.linting.enabled": true,
  "python.linting.pylintEnabled": true,
  "python.testing.pytestEnabled": true,
  "python.testing.pytestArgs": ["python/tests"],
  
  "typescript.preferences.includePackageJsonAutoImports": "on",
  "typescript.suggest.autoImports": true,
  "typescript.updateImportsOnFileMove.enabled": "always",
  
  "files.exclude": {
    "**/node_modules": true,
    "**/venv": true,
    "**/__pycache__": true,
    "**/*.pyc": true,
    "**/shared/models/*.pt": true,
    "**/shared/data/*.csv": true
  },
  
  "search.exclude": {
    "**/node_modules": true,
    "**/venv": true,
    "**/shared/data": true,
    "**/shared/models": true
  },
  
  "files.associations": {
    "*.yml": "yaml",
    "*.yaml": "yaml",
    "Dockerfile*": "dockerfile"
  },
  
  "terminal.integrated.env.linux": {
    "PYTHONPATH": "${workspaceFolder}/python/src"
  },
  
  "terminal.integrated.cwd": "${workspaceFolder}",
  "terminal.integrated.defaultProfile.linux": "zsh",
  
  "jupyter.notebookFileRoot": "${workspaceFolder}/python",
  "jupyter.interactiveWindow.textEditor.executeSelection": true,
  
  "git.ignoreLimitWarning": true,
  "git.autofetch": true,
  
  "docker.showStartPage": false,
  
  "[python]": {
    "editor.formatOnSave": true,
    "editor.codeActionsOnSave": {
      "source.organizeImports": true
    }
  },
  
  "[typescript]": {
    "editor.formatOnSave": true,
    "editor.codeActionsOnSave": {
      "source.organizeImports": true
    }
  },
  
  "[json]": {
    "editor.defaultFormatter": "esbenp.prettier-vscode"
  },
  
  "remote.WSL.fileWatcher.polling": false,
  "files.watcherExclude": {
    "**/shared/data/**": true,
    "**/shared/models/**": true
  }
}
```

## Cross-Platform File Operations

### **Best Practices for Each Direction**

**Ubuntu → Windows (`/mnt/c/`):**
```bash
# ✅ Good: Quick file access, copying
cp /mnt/c/Users/username/Downloads/dataset.csv ~/projects/

# ✅ Good: Running Windows executables
/mnt/c/Program\ Files/Git/bin/git.exe --version

# ❌ Avoid: Intensive file operations in /mnt/c/
# This is slow:
cd /mnt/c/dev/my-project/
npm install  # Better to do in Ubuntu filesystem
```

**Windows → Ubuntu (`\\wsl.localhost\`):**
```cmd
REM ✅ Good: Opening Ubuntu files in Windows editors
code "\\wsl.localhost\Ubuntu-24.04\home\username\projects\"

REM ✅ Good: File Explorer browsing
explorer "\\wsl.localhost\Ubuntu-24.04\home\username\"

REM ✅ Good: Copying files to Windows
copy "\\wsl.localhost\Ubuntu-24.04\home\username\output.json" "C:\temp\"
```

### **Symbolic Links for Convenience**

**Create shortcuts between systems:**
```bash
# From Ubuntu: Easy Windows access
ln -s /mnt/c/Users/username/Documents ~/windows-docs
ln -s /mnt/c/Users/username/Desktop ~/windows-desktop

# Now you can use:
cd ~/windows-docs  # Instead of cd /mnt/c/Users/username/Documents
```

**From Windows: Pin common Ubuntu locations**
1. Pin `\\wsl.localhost\Ubuntu-24.04\home\username\` to Quick Access
2. Create desktop shortcuts to specific Ubuntu project folders

## Daily Development Workflow

**Morning Setup (Modern WSL2):**
```bash
# Navigate to Ubuntu project directory
cd ~/dev/ai-projects/my-ai-app

# Activate Python environment
cd python && source venv/bin/activate

# Check for updates
git pull
pip install -r requirements.txt

# Start development servers
cd ../typescript && npm install
npm run dev &  # Background process

# Open VSCode (opens automatically in WSL mode)
code ~/dev/ai-projects/my-ai-app
```

**Cross-Platform File Sharing (When Needed):**
```bash
# Create symbolic links for easy Windows access to specific files
ln -s ~/dev/ai-projects/my-ai-app/shared/configs /mnt/c/Users/username/Desktop/ai-configs

# Or create a shared directory for large file transfers
mkdir -p /mnt/c/ai-shared
ln -s /mnt/c/ai-shared ~/shared-windows

# Access from Windows: C:\ai-shared\
# Access from Ubuntu: ~/shared-windows/
```

## Quick Access Setup

**Pin to Windows Quick Access:**
1. Open File Explorer
2. Navigate to: `\\wsl.localhost\Ubuntu-24.04\home\username\dev\`
3. Right-click → "Pin to Quick access"

**Create Desktop Shortcut:**
```batch
@echo off
REM Save as "AI Projects.bat" on Desktop
explorer "\\wsl.localhost\Ubuntu-24.04\home\%USERNAME%\dev\ai-projects"
```

**VSCode Workspace File (Optional):**
```json
{
  "folders": [
    {
      "name": "AI Projects Root",
      "path": "\\\\wsl.localhost\\Ubuntu-24.04\\home\\username\\dev\\ai-projects"
    }
  ],
  "settings": {
    "python.defaultInterpreterPath": "./python/venv/bin/python"
  }
}
```

## Key Insights and Best Practices

### **The Fundamental Pattern:**
- **Ubuntu sees Windows** as mounted drives (`/mnt/c/`) - Linux philosophy
- **Windows sees Ubuntu** as a network location (`\\wsl.localhost\`) - Windows philosophy
- **Both approaches** preserve their respective filesystem conventions
- **Performance characteristics** differ based on direction and operation type

### **Performance Optimization:**
- **Use Ubuntu native filesystem** (`~/dev/`) for development work
- **Access via `\\wsl.localhost\`** from Windows for seamless integration
- **Avoid `/mnt/c/`** for intensive operations like compilation
- **Use symbolic links** for convenient cross-platform shortcuts

### **Modern WSL2 Advantages:**
- **Windows 11 integration** is seamless via `\\wsl.localhost\`
- **VSCode automatically** detects WSL projects and switches to WSL mode
- **File watching works** correctly with Ubuntu native filesystem
- **Git performance** is much faster on Ubuntu native filesystem
- **Node.js/Python compilation** is significantly faster in Ubuntu filesystem

This architecture maximizes performance while maintaining easy Windows access through modern Windows 11 + WSL2 integration patterns.