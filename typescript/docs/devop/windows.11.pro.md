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

**Option A: Docker Desktop (Recommended)**
- Download and install Docker Desktop for Windows
- During installation, ensure "Use WSL 2 instead of Hyper-V" is enabled
- In Docker Desktop settings, enable WSL 2 integration for your Ubuntu distribution

**Option B: Docker CE directly in WSL2**
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

# Add your user to docker group
sudo usermod -aG docker $USER

# Start Docker service
sudo service docker start

# Enable Docker to start on boot
echo 'sudo service docker start' >> ~/.bashrc
```

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
# Set your identity
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

# Set up your API key (you'll need an Anthropic API key)
claude-code config set api-key YOUR_API_KEY

# Test the installation
claude-code --help
```

**Note:** Since Claude Code is relatively new, I recommend checking the official documentation at https://docs.anthropic.com/en/docs/claude-code for the most up-to-date installation instructions and setup process.

## 9. Additional Useful Tools for AI Development

**Install additional development tools:**
```bash
# Essential build tools
sudo apt install build-essential curl wget unzip

# Modern terminal tools
sudo apt install zsh fish tmux htop tree

# Install Oh My Zsh (optional, for better terminal experience)
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Install GitHub CLI
curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
sudo chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
sudo apt update
sudo apt install gh

# Authenticate with GitHub
gh auth login
```

## 10. VSCode Integration Updates

**Add these extensions for the new tools:**
- Docker (ms-azuretools.vscode-docker)
- GitLens (eamodio.gitlens) 
- GitHub Pull Requests and Issues (GitHub.vscode-pull-request-github)

**Update your `.bashrc` for better development experience:**
```bash
# Add to ~/.bashrc

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
source ~/.bashrc
```

## 11. Testing Your Setup

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

## 12. Installing and Configuring Zsh + Oh My Zsh

**Install Zsh:**
```bash
# Update package list
sudo apt update

# Install zsh
sudo apt install zsh

# Check installation
zsh --version
```

**Install Oh My Zsh:**
```bash
# Install Oh My Zsh
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# This will:
# - Download and install Oh My Zsh
# - Backup your existing ~/.zshrc
# - Set zsh as your default shell
# - Start a new zsh session
```

**If it doesn't set zsh as default automatically:**
```bash
# Set zsh as default shell
chsh -s $(which zsh)

# Or if that doesn't work:
sudo chsh -s $(which zsh) $USER
```

## 13. Essential Oh My Zsh Configuration

**Edit your `~/.zshrc` file:**
```bash
code ~/.zshrc
```

**Recommended configuration:**
```bash
# Path to your oh-my-zsh installation
export ZSH="$HOME/.oh-my-zsh"

# Theme (try different ones!)
ZSH_THEME="robbyrussell"  # or "powerlevel10k/powerlevel10k" for advanced

# Plugins (add gradually to avoid conflicts)
plugins=(
    git
    docker
    docker-compose
    node
    npm
    python
    pip
    vscode
    zsh-autosuggestions
    zsh-syntax-highlighting
    conda-zsh-completion
)

source $ZSH/oh-my-zsh.sh

# Custom aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Development aliases
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git pull'
alias gb='git branch'
alias gco='git checkout'

# Docker aliases
alias dps='docker ps'
alias di='docker images'
alias dc='docker-compose'

# Python aliases
alias py='python3'
alias pip='pip3'

# Navigation aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# WSL-specific aliases
alias windir='cd /mnt/c/Users/$USER'
alias devdir='cd /mnt/c/dev'

# Auto-start Docker service
if ! service docker status > /dev/null 2>&1; then
    sudo service docker start > /dev/null 2>&1
fi

# Add local bin to PATH
export PATH="$HOME/.local/bin:$PATH"

# Python path management
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# Node version manager
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
```

## 14. Install Essential Zsh Plugins

**Install additional plugins manually:**
```bash
# zsh-autosuggestions (suggests commands as you type)
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# zsh-syntax-highlighting (highlights commands)
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

# conda-zsh-completion (if you use conda)
git clone https://github.com/esc/conda-zsh-completion ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/conda-zsh-completion
```

## 15. Advanced Theme: Powerlevel10k (Optional)

**Install Powerlevel10k theme:**
```bash
# Install the theme
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k

# Install required fonts
sudo apt install fonts-powerline

# For Windows Terminal, also install Nerd Fonts
# Download from: https://www.nerdfonts.com/font-downloads
# Recommended: MesloLGS NF
```

**Update ~/.zshrc:**
```bash
ZSH_THEME="powerlevel10k/powerlevel10k"
```

**Configure Powerlevel10k:**
```bash
# Restart zsh and run configuration wizard
exec zsh
p10k configure

# Or reconfigure anytime with:
p10k configure
```

## 16. Windows Terminal Integration

**For better experience in Windows Terminal, add this to your settings.json:**
```json
{
    "profiles": {
        "list": [
            {
                "guid": "{07b52e3e-de2c-5db4-bd2d-ba144ed6c273}",
                "name": "Ubuntu (WSL2)",
                "source": "Windows.Terminal.Wsl",
                "startingDirectory": "//wsl$/Ubuntu/home/YOUR_USERNAME",
                "fontFace": "MesloLGS NF",
                "fontSize": 11,
                "colorScheme": "One Half Dark"
            }
        ]
    }
}
```

## 17. Useful Zsh Features and Tips

**Enable some Oh My Zsh features in ~/.zshrc:**
```bash
# Case-sensitive completion
# CASE_SENSITIVE="true"

# Hyphen-insensitive completion
HYPHEN_INSENSITIVE="true"

# Auto-update behavior
zstyle ':omz:update' mode auto
zstyle ':omz:update' frequency 13

# Disable auto-setting terminal title
DISABLE_AUTO_TITLE="true"

# Enable command auto-correction
ENABLE_CORRECTION="true"

# Display red dots while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Disable marking untracked files under VCS as dirty
DISABLE_UNTRACKED_FILES_DIRTY="true"
```

**Custom functions you can add to ~/.zshrc:**
```bash
# Quick directory creation and navigation
mkcd() {
    mkdir -p "$1" && cd "$1"
}

# Extract any archive
extract() {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1     ;;
            *.tar.gz)    tar xzf $1     ;;
            *.bz2)       bunzip2 $1     ;;
            *.rar)       unrar e $1     ;;
            *.gz)        gunzip $1      ;;
            *.tar)       tar xf $1      ;;
            *.tbz2)      tar xjf $1     ;;
            *.tgz)       tar xzf $1     ;;
            *.zip)       unzip $1       ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1        ;;
            *)     echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# Find and kill process
fkill() {
    local pid
    pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
    if [ "x$pid" != "x" ]; then
        echo $pid | xargs kill -${1:-9}
    fi
}
```

## 18. Apply Changes

**Reload your configuration:**
```bash
# Reload zsh configuration
source ~/.zshrc

# Or restart zsh session
exec zsh
```

## 19. Test Your Setup

**Test various features:**
```bash
# Test autosuggestions (start typing and use right arrow to accept)
git sta[TAB]

# Test syntax highlighting (commands should be colored)
ls -la

# Test aliases
gs  # should run git status

# Test custom function
mkcd test-dir  # should create and enter directory
```

Your terminal should now have:
- **Autosuggestions**: Suggests commands as you type
- **Syntax highlighting**: Colors commands, paths, etc.
- **Better tab completion**: More intelligent completions
- **Git integration**: Shows branch info, status in prompt
- **Useful aliases**: Shortcuts for common commands
- **Custom functions**: Enhanced functionality
