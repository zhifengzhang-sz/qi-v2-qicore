I'll search for the latest information on Ollama GPU setup, particularly for Ubuntu WSL2 environments.Based on my research of the latest Ollama documentation and community discussions, here's a comprehensive, updated guide for setting up Ollama with GPU acceleration in Ubuntu WSL2:

## **Updated Ollama GPU Setup Guide for WSL2 (2025)**

### **Prerequisites & System Requirements**

**Essential Requirements:**
- Windows 11 or Windows 10 version 21H2+ with WSL2 enabled
- NVIDIA GPU with compute capability 5.0+ (check at: https://developer.nvidia.com/cuda-gpus)
- NVIDIA driver version 531 or newer
- At least 8GB RAM (16GB+ recommended)

### **Step 1: GPU Driver Setup**

**Update Windows GPU Drivers:**
1. Download the latest NVIDIA drivers from NVIDIA's website (NOT Windows Update)
2. Install the drivers on Windows (this automatically enables WSL2 GPU passthrough)
3. Restart your computer

**Verify GPU Access in WSL2:**
```bash
nvidia-smi
```
This should show your GPU details and CUDA version.

### **Step 2: Install CUDA in WSL2**

**Install CUDA Toolkit (WSL2-Ubuntu):**
```bash
# Remove any existing CUDA installations
sudo apt remove --purge cuda* nvidia-cuda-*
sudo apt autoremove

# Download and install CUDA for WSL2
wget https://developer.download.nvidia.com/compute/cuda/repos/wsl-ubuntu/x86_64/cuda-wsl-ubuntu.pin
sudo mv cuda-wsl-ubuntu.pin /etc/apt/preferences.d/cuda-repository-pin-600

# Download CUDA repo (adjust version as needed)
wget https://developer.download.nvidia.com/compute/cuda/12.6.2/local_installers/cuda-repo-wsl-ubuntu-12-6-local_12.6.2-1_amd64.deb
sudo dpkg -i cuda-repo-wsl-ubuntu-12-6-local_12.6.2-1_amd64.deb
sudo cp /var/cuda-repo-wsl-ubuntu-12-6-local/cuda-*-keyring.gpg /usr/share/keyrings/

# Install CUDA
sudo apt update
sudo apt install -y cuda-toolkit-12-6
```

### **Step 3: Install and Configure Ollama**

**Install Ollama:**
```bash
curl -fsSL https://ollama.com/install.sh | sh
```

**Critical Configuration for WSL2:**
WSL2 only supports GPUs via nvidia passthrough, so Ollama checks for nvidia-smi to determine if GPU is available

### **Step 4: Essential Environment Variables**

**Add these to your `~/.bashrc` or `~/.zshrc`:**
```bash
# GPU visibility and optimization
export CUDA_VISIBLE_DEVICES=0
export OLLAMA_GPU_LAYERS=35  # Adjust based on your GPU VRAM
export OLLAMA_FLASH_ATTENTION=1

# Memory management
export OLLAMA_MAX_LOADED_MODELS=1
export OLLAMA_NUM_PARALLEL=4
export OLLAMA_GPU_OVERHEAD=0.5  # Reserve 0.5GB for GPU overhead

# Performance tuning
export OLLAMA_KV_CACHE_TYPE="q8_0"
export OLLAMA_KEEP_ALIVE=5m

# Reload configuration
source ~/.bashrc
```

### **Step 5: Configure WSL2 Memory Settings**

**Edit `%USERPROFILE%\.wslconfig` on Windows:**
```ini
[wsl2]
memory=16GB  # Adjust based on your RAM
processors=8
swap=2GB
nestedVirtualization=true
```

**Restart WSL2:**
```cmd
wsl --shutdown
wsl
```

### **Step 6: GPU Detection Troubleshooting**

**If Ollama shows "No NVIDIA GPU detected":**

1. **Check UVM Driver:**
   ```bash
   sudo nvidia-modprobe -u
   
   # If issues persist, reload UVM driver
   sudo rmmod nvidia_uvm
   sudo modprobe nvidia_uvm
   ```

2. **Verify Container Runtime (if using Docker):**
   ```bash
   docker run --gpus all ubuntu nvidia-smi
   ```

3. **Check for GPU Libraries:**
   ```bash
   ls -la /usr/lib/wsl/lib/
   ls -la /usr/lib/x86_64-linux-gnu/libnvidia-ml.so*
   ```

4. **Force GPU Detection:**
   ```bash
   export OLLAMA_DEBUG=1
   ollama serve
   ```

### **Step 7: Performance Optimization**

**Model-specific GPU Layer Configuration:**
Model weights are static and predictable. If you set --n-gpu-layers 80, Ollama tries to fit 80 layers of the model onto GPU VRAM

**Calculate optimal layers:**
```bash
# For 8GB VRAM GPU, reserve 2GB for system overhead
# Example: 7B model ≈ 4GB, can fit ~35 layers
export OLLAMA_GPU_LAYERS=35

# For 16GB VRAM GPU
export OLLAMA_GPU_LAYERS=65
```

**Hybrid CPU/GPU Configuration:**
```bash
# Run model with specific GPU layers
ollama run llama3.3:8b --gpu-layers 20
```

### **Step 8: Monitoring and Verification**

**Check GPU Usage:**
```bash
# In one terminal
watch -n 1 nvidia-smi

# In another terminal
ollama run llama3.3:8b
```

**Performance Monitoring:**
Debug mode provides detailed information about memory usage, GPU utilization, and request processing times

```bash
export OLLAMA_DEBUG=1
export OLLAMA_VERBOSE=1
ollama serve
```

### **Common Issues and Solutions**

**Issue: Low GPU Usage (10-20%)**
- The huge ctx-size and batch-size mean Ollama reserves a lot of VRAM for the KV cache. It reduces the number of GPU layers to avoid running out of space during generation
- **Solution:** Reduce context size or increase GPU layers

**Issue: "WARNING: No NVIDIA GPU detected"**
- **Solution:** reload the NVIDIA UVM driver with the command: sudo rmmod nvidia_uvm && sudo modprobe nvidia_uvm

**Issue: Poor Performance**
- **Solution:** Fine-tune GPU utilization for different scenarios: export CUDA_VISIBLE_DEVICES=0 export OLLAMA_GPU_OVERHEAD=1536000000 # 1.5GB overhead

### **Latest Environment Variables (2025)**

```bash
# Complete optimization configuration
export CUDA_VISIBLE_DEVICES=0
export OLLAMA_GPU_LAYERS=35
export OLLAMA_FLASH_ATTENTION=1
export OLLAMA_MAX_LOADED_MODELS=2
export OLLAMA_NUM_PARALLEL=4
export OLLAMA_GPU_OVERHEAD=0.5
export OLLAMA_KV_CACHE_TYPE="q8_0"
export OLLAMA_KEEP_ALIVE=5m
export OLLAMA_DEBUG=1  # Remove after setup
```

### **Alternative: Docker Approach**

If native installation fails, use Docker:
```bash
# Install Docker and NVIDIA Container Toolkit
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# Run Ollama in Docker
docker run -d --gpus=all -v ollama:/root/.ollama -p 11434:11434 --name ollama ollama/ollama
```

This updated guide incorporates the latest Ollama features and addresses common WSL2-specific GPU detection issues that have been reported in 2025.