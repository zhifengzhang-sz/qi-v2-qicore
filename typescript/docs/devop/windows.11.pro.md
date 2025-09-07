## 1. Setting up WSL2/Ubuntu

**Quick Install Method (2025 - Recommended):**
```powershell
# Run PowerShell as Administrator
# This single command enables WSL features and installs Ubuntu automatically
wsl --install

# Optional: Install without distribution for more control
wsl --install --no-distribution

# Optional: Install specific distribution
wsl --install -d Ubuntu
```

**Alternative Manual Method (if quick install fails):**
```powershell
# Enable WSL and Virtual Machine Platform
dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart

# Restart computer, then set WSL2 as default
wsl --set-default-version 2
```

**Update Ubuntu after first setup:**
```bash
sudo apt update && sudo apt upgrade -y
```

## 2. Setting up VSCode for WSL2 Development

**Install VSCode Extensions:**
- Install VSCode on Windows (version 1.35 or later)
- Install the "Remote - WSL" extension (ms-vscode-remote.remote-wsl)
- Or install "Remote Development" extension pack (includes WSL, SSH, and Dev Containers)

**Connect VSCode to WSL2:**
```bash
# From within WSL2 Ubuntu terminal
code .
```

This automatically installs VSCode Server in WSL2 and opens VSCode connected to the Ubuntu environment.

**Recommended WSL2 Extensions:**
- Python (ms-python.python)
- Pylance (ms-python.vscode-pylance)
- TypeScript and JavaScript Language Features
- Git Extension Pack

## 3. Python Development Environment (Hybrid Setup)

**In WSL2 Ubuntu (for editing/tools):**
```bash
# Install Python build dependencies (2025 updated list)
sudo apt install -y gcc make build-essential libssl-dev libffi-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev liblzma-dev

# Install pyenv for Python version management
curl https://pyenv.run | bash

# Add to ~/.bashrc (or ~/.zshrc if using zsh)
echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.bashrc
echo '[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bashrc
echo 'eval "$(pyenv init - bash)"' >> ~/.bashrc

# Also create ~/.profile with the same commands
echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.profile
echo '[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.profile
echo 'eval "$(pyenv init -)"' >> ~/.profile

# Reload configuration
exec "$SHELL"

# Install Python versions we need
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
# Remove any existing Node.js installations first (recommended)
sudo apt remove nodejs npm

# Install curl if not installed
sudo apt-get install curl

# Install Node.js using nvm (2025 method)
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/master/install.sh | bash

# Restart shell or source configuration
exec "$SHELL"
# Or: source ~/.bashrc

# Verify nvm installation
command -v nvm

# Install latest LTS Node.js
nvm install --lts
nvm use --lts

# Verify installation
node --version
npm --version

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
# In the shared project directory
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

## 5. Additional Tips for AI Development

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

---

## 6. Docker in WSL2

**Option A: Docker Desktop (Recommended for most users)**
- Download and install Docker Desktop for Windows
- During installation, ensure "Use WSL 2 instead of Hyper-V" is enabled
- In Docker Desktop settings, enable WSL 2 integration for your Ubuntu distribution
- **Note**: Requires paid license for enterprise use (free for personal use)
- Provides built-in Kubernetes, cross-platform integration, and easier setup

**Option B: Docker CE directly in WSL2 (Enterprise/cost-conscious choice)**
```bash
# Update package index
sudo apt update

# Install prerequisites
sudo apt install apt-transport-https ca-certificates curl gnupg lsb-release

# Add Docker's official GPG key
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg

# Add Docker repository
echo "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

# Install Docker CE
sudo apt update
sudo apt install docker-ce docker-ce-cli containerd.io docker-compose-plugin

# Add the user to docker group
sudo usermod -aG docker $USER

# Start Docker service (WSL2 doesn't use systemd by default)
sudo service docker start

# Auto-start Docker (add to shell config)
echo 'if ! service docker status > /dev/null 2>&1; then' >> ~/.bashrc
echo '    sudo service docker start > /dev/null 2>&1' >> ~/.bashrc
echo 'fi' >> ~/.bashrc
```

**Docker CE Benefits**: No licensing costs, more control, native Linux experience  
**Docker CE Challenges**: Requires more Linux knowledge, manual daemon management

**Test Docker:**
```bash
# Restart WSL2 session or run:
newgrp docker

# Test Docker
docker run hello-world
```

## 7. Git Setup

**Install Git:**
```bash
# Install Git (usually pre-installed, but ensure latest version)
sudo apt update
sudo apt install git
```

**Configure Git:**
```bash
# Set our identity
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"

# Set default branch name
git config --global init.defaultBranch main

# Improve Git experience
git config --global pull.rebase false
git config --global core.autocrlf input
git config --global core.editor "code --wait"
```

**SSH Key Setup (recommended):**
```bash
# Generate SSH key
ssh-keygen -t ed25519 -C "your.email@example.com"

# Start SSH agent
eval "$(ssh-agent -s)"

# Add SSH key
ssh-add ~/.ssh/id_ed25519

# Display public key (add this to GitHub/GitLab)
cat ~/.ssh/id_ed25519.pub
```

**Git Credential Helper:**
```bash
# Use Git Credential Manager from Windows
git config --global credential.helper "/mnt/c/Program\ Files/Git/mingw64/bin/git-credential-manager.exe"
```

## 8. Claude Code CLI

**Installation:**
```bash
# Install Claude Code CLI
# Note: Check the official documentation for the latest installation method
curl -fsSL https://raw.githubusercontent.com/anthropics/claude-code/main/install.sh | bash

# Or using npm if available:
npm install -g @anthropic/claude-code

# Or using pip if available:
pip install claude-code
```

**Setup and Configuration:**
```bash
# Initialize Claude Code (follow prompts for API key setup)
claude-code init

# Set up the API key (we'll need an Anthropic API key)
claude-code config set api-key YOUR_API_KEY

# Test the installation
claude-code --help
```

**Note:** Check the official documentation at https://docs.anthropic.com/en/docs/claude-code for the most up-to-date installation instructions and setup process. As of 2025, Claude Code CLI is actively maintained and regularly updated.

## 9. Terminal Enhancement with Zsh and Oh My Zsh (2025 Updated)

**Install Zsh and Oh My Zsh:**
```bash
# Update package list and install essential tools
sudo apt update
sudo apt install build-essential curl wget unzip zsh tmux htop tree

# Install Oh My Zsh
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Set zsh as default shell if not set automatically
chsh -s $(which zsh)
```

**Install Powerlevel10k Theme (2025 Method):**
```bash
# Clone Powerlevel10k repository
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k

# Install recommended fonts (MesloLGS NF)
sudo apt install fonts-powerline

# For Windows Terminal, also install MesloLGS NF fonts manually
# Download from: https://github.com/romkatv/powerlevel10k#fonts
```

**Install Essential Plugins:**
```bash
# Auto-suggestions plugin
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# Syntax highlighting plugin
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
```

**Configure Zsh (~/.zshrc):**
```bash
# Edit zsh configuration
code ~/.zshrc

# Set Powerlevel10k theme
ZSH_THEME="powerlevel10k/powerlevel10k"

# Enable plugins (2025 recommended set)
plugins=(git docker docker-compose node npm python pip vscode zsh-autosuggestions zsh-syntax-highlighting)

# Reload configuration
source ~/.zshrc

# Run configuration wizard (will auto-start on first load)
p10k configure
```

**Windows Terminal Font Setup:**
- Set font to "MesloLGS NF" in Windows Terminal settings
- Font size: 11-12 recommended
- This enables all Powerlevel10k icons and styling

## 10. Additional Development Tools

**Install GitHub CLI:**
```bash
curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
sudo chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
sudo apt update
sudo apt install gh

# Authenticate with GitHub
gh auth login
```

## 11. VSCode Integration Updates

**Add these extensions for the new tools:**
- Docker (ms-azuretools.vscode-docker)
- GitLens (eamodio.gitlens) 
- GitHub Pull Requests and Issues (GitHub.vscode-pull-request-github)

**Update your `.zshrc` for better development experience:**
```bash
# Add to ~/.zshrc

# Docker aliases
alias dps='docker ps'
alias di='docker images'
alias dc='docker-compose'

# Git aliases
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git pull'
alias gb='git branch'
alias gco='git checkout'

# Claude Code alias (adjust based on actual command name)
alias cc='claude-code'

# Auto-start Docker service
if ! service docker status > /dev/null 2>&1; then
    sudo service docker start > /dev/null 2>&1
fi
```

**Reload your configuration:**
```bash
source ~/.zshrc
# or
exec zsh
```

## 12. Testing Our Setup

**Create a test project to verify everything works:**
```bash
# Create test directory
mkdir -p /mnt/c/dev/test-setup
cd /mnt/c/dev/test-setup

# Initialize git repo
git init

# Create simple Python file
echo "print('Hello from WSL2!')" > hello.py

# Create Dockerfile
cat << EOF > Dockerfile
FROM python:3.11-slim
COPY hello.py .
CMD ["python", "hello.py"]
EOF

# Test Docker build
docker build -t test-setup .

# Test Docker run
docker run test-setup

# Test Claude Code (if properly configured)
claude-code "Help me optimize this Python script"

# Open in VSCode
code .
```

---

This completes the Windows 11 Pro setup with WSL2, providing a comprehensive development environment with enhanced terminal capabilities through Zsh and Oh My Zsh integration.
