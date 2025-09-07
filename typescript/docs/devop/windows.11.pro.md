## 1. Setting up WSL2/Ubuntu

**Enable WSL2:**
```powershell
# Run PowerShell as Administrator
dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart
```

**Restart your computer, then:**
```powershell
# Set WSL2 as default version
wsl --set-default-version 2

# Install Ubuntu (latest LTS)
wsl --install -d Ubuntu
```

**Update Ubuntu after first setup:**
```bash
sudo apt update && sudo apt upgrade -y
```

## 2. Setting up VSCode for WSL2 Development

**Install VSCode Extensions:**
- Install VSCode on Windows
- Install the "WSL" extension (ms-vscode-remote.remote-wsl)
- Install "Remote Development" extension pack

**Connect VSCode to WSL2:**
```bash
# From within WSL2 Ubuntu terminal
code .
```

This automatically installs VSCode Server in WSL2 and opens VSCode connected to your Ubuntu environment.

**Recommended WSL2 Extensions:**
- Python (ms-python.python)
- Pylance (ms-python.vscode-pylance)
- TypeScript and JavaScript Language Features
- Git Extension Pack

## 3. Python Development Environment (Hybrid Setup)

**In WSL2 Ubuntu (for editing/tools):**
```bash
# Install Python build dependencies
sudo apt install python3-pip python3-venv python3-dev build-essential

# Install pyenv for Python version management
curl https://pyenv.run | bash

# Add to ~/.bashrc
echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.bashrc
echo 'command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bashrc
echo 'eval "$(pyenv init -)"' >> ~/.bashrc

# Reload bash
source ~/.bashrc

# Install Python versions you need
pyenv install 3.11.5
pyenv install 3.10.12
pyenv global 3.11.5
```

**On Windows (for execution with GPU):**
- Install Python from python.org or Microsoft Store
- Install CUDA Toolkit if using NVIDIA GPUs
- Install PyTorch/TensorFlow with CUDA support

**Shared Project Setup:**
```bash
# In WSL2, create a shared workspace
mkdir -p /mnt/c/dev/ai-projects
cd /mnt/c/dev/ai-projects

# Create virtual environment that both can access
python3 -m venv venv

# WSL2 activation
source venv/bin/activate

# Windows activation (from CMD/PowerShell in C:\dev\ai-projects)
# venv\Scripts\activate
```

**VSCode Configuration for Hybrid Python:**
Create `.vscode/settings.json` in your project:
```json
{
    "python.pythonPath": "/mnt/c/dev/ai-projects/venv/bin/python",
    "python.terminal.activateEnvironment": true,
    "python.defaultInterpreterPath": "/mnt/c/dev/ai-projects/venv/bin/python"
}
```

**Execution Workflow:**
- Edit in VSCode connected to WSL2
- Run development/testing in WSL2 terminal
- Execute GPU-intensive training on Windows PowerShell/CMD

## 4. TypeScript Development Environment

**In WSL2 Ubuntu:**
```bash
# Install Node.js using nvm
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
source ~/.bashrc

# Install latest LTS Node.js
nvm install --lts
nvm use --lts

# Install global TypeScript tools
npm install -g typescript ts-node @types/node

# For AI/ML TypeScript projects
npm install -g @tensorflow/tfjs-node
```

**On Windows (for GPU execution if needed):**
- Install Node.js from nodejs.org
- Install the same global packages

**Project Setup:**
```bash
# In your shared project directory
cd /mnt/c/dev/ai-projects/my-ts-project

# Initialize TypeScript project
npm init -y
npm install -D typescript @types/node ts-node
npx tsc --init

# Install AI/ML libraries
npm install @tensorflow/tfjs @tensorflow/tfjs-node
```

**VSCode TypeScript Configuration:**
Add to `.vscode/settings.json`:
```json
{
    "typescript.preferences.includePackageJsonAutoImports": "on",
    "typescript.suggest.autoImports": true,
    "typescript.updateImportsOnFileMove.enabled": "always"
}
```

## Additional Tips for AI Development

**Jupyter Setup:**
```bash
# In WSL2
pip install jupyter jupyterlab notebook

# Access from Windows browser
jupyter lab --ip=0.0.0.0 --port=8888 --no-browser --allow-root
```

**GPU Monitoring:**
```powershell
# Windows PowerShell
nvidia-smi
```

**File System Notes:**
- Windows files: `/mnt/c/` in WSL2
- WSL2 files: `\\wsl$\Ubuntu\home\username\` in Windows Explorer
- Use `/mnt/c/dev/` for shared projects to ensure both environments can access files efficiently

**Performance Tips:**
- Keep frequently accessed files on the WSL2 filesystem for better performance
- Use shared directories (`/mnt/c/`) for projects you need to access from both environments
- Install packages in the environment where you'll primarily use them

This setup gives you the flexibility to develop in a Linux environment while leveraging Windows GPU resources for AI training and inference. The shared file system approach ensures seamless workflow between both environments.