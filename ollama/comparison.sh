#!/bin/bash
# Test all three approaches
models=("deepseek-coder:6.7b" "qwen2.5-coder:7b" "qwen3:14b")
prompt="Write a Python function to implement a thread-safe LRU cache"

for model in "${models[@]}"; do
    echo "=== Testing $model ==="
    time ollama run $model "$prompt"
done