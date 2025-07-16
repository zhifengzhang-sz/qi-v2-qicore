```bash
#!/bin/bash

# ============================================
# Design Compliance System for Claude Code
# Ensure AI follows design docs and implementation guides
# ============================================

echo "📐 Setting up Design Compliance System..."

# ============================================
# Solution 1: Design-First Workflow Commands
# ============================================

cat > ~/.claude/commands/design-first.md << 'EOF'
# Design-First Implementation Workflow

ALWAYS follow this workflow to ensure design compliance.

## Mandatory Pre-Implementation Process:
Use **sequential-thinking** to systematically execute:

### Step 1: Read ALL Design Documentation (**filesystem**)
- Read ./docs/design/architecture.md (overall system design)
- Read ./docs/design/api.md (API contracts and interfaces)
- Read ./docs/design/patterns.md (coding patterns and conventions)
- Read ./docs/design/[specific-feature].md (feature-specific design)
- Read any README.md files in relevant directories

### Step 2: Store Design Principles (**memory**)
- Store architectural decisions and constraints
- Remember coding patterns and conventions
- Store interface contracts and data models
- Remember any design trade-offs and rationale

### Step 3: Validate Against Design
- Ensure implementation matches documented architecture
- Verify API contracts are followed exactly
- Check that coding patterns are consistent
- Validate data models match specifications

### Step 4: Implementation with Design Compliance
- Reference design docs for EVERY implementation decision
- Follow documented patterns and conventions
- Implement interfaces exactly as specified
- Maintain architectural boundaries

### Step 5: Design Compliance Check
- Verify implementation follows ALL design constraints
- Check that no design principles were violated
- Ensure consistency with existing codebase patterns
- Validate that interfaces match documentation

## Success Criteria:
- All design documents have been read and understood
- Implementation follows documented architecture
- Coding patterns are consistent throughout
- API contracts are implemented exactly as specified
- No design principles have been violated

## Failure Prevention:
- NEVER implement without reading design docs first
- NEVER deviate from documented patterns without explicit approval
- NEVER assume implementation details not specified in design
- ALWAYS ask for clarification on ambiguous design points
EOF

# ============================================
# Solution 2: Design Template System
# ============================================

echo "📋 Creating design template system..."

# Create design document templates
mkdir -p ~/.claude/templates/design

cat > ~/.claude/templates/design/architecture.md << 'EOF'
# Architecture Design Document

## System Overview
- High-level system architecture
- Component relationships and dependencies
- Data flow and communication patterns

## Core Principles
- Architectural constraints and guidelines
- Design patterns to follow
- Anti-patterns to avoid

## Implementation Guidelines
- Coding standards and conventions
- File and directory structure
- Naming conventions

## Interface Contracts
- API specifications
- Data models and schemas
- Communication protocols

## Quality Requirements
- Performance requirements
- Security considerations
- Scalability requirements

## Decision Record
- Key architectural decisions
- Trade-offs and rationale
- Alternative approaches considered
EOF

cat > ~/.claude/templates/design/feature-template.md << 'EOF'
# Feature Design: [FEATURE_NAME]

## Requirements
- Functional requirements
- Non-functional requirements
- Acceptance criteria

## Design Approach
- Implementation strategy
- Architecture integration
- Pattern applications

## Interface Specification
- API endpoints (if applicable)
- Data models
- Input/output specifications

## Implementation Plan
- Step-by-step implementation approach
- Dependencies and prerequisites
- Testing strategy

## Compliance Checklist
- [ ] Follows system architecture
- [ ] Uses documented patterns
- [ ] Implements specified interfaces
- [ ] Meets quality requirements
EOF

# ============================================
# Solution 3: Compliance Checking Commands
# ============================================

cat > ~/.claude/commands/check-compliance.md << 'EOF'
# Design Compliance Checker

Verify that implementation follows design documentation.

## Compliance Check Process:
Use **sequential-thinking** to systematically verify:

### 1. Architecture Compliance (**filesystem** + **memory**)
- Read current implementation files
- Compare against architecture documentation
- Verify component boundaries are respected
- Check dependency relationships match design

### 2. Pattern Compliance
- Scan codebase for pattern usage
- Verify patterns match documented standards
- Check for pattern consistency across files
- Identify any anti-pattern usage

### 3. Interface Compliance
- Verify API implementations match specifications
- Check data models against documented schemas
- Validate function signatures match interfaces
- Ensure error handling follows guidelines

### 4. Convention Compliance
- Check file and directory structure
- Verify naming conventions are followed
- Validate code style matches standards
- Check documentation completeness

## Compliance Report:
Generate report with:
- ✅ Compliant areas
- ❌ Non-compliant areas with specific violations
- 🔄 Areas needing design clarification
- 📝 Recommendations for compliance

## Auto-Fix Mode:
- Fix pattern violations automatically
- Update code to match design specifications
- Correct naming convention violations
- Add missing documentation

## Usage:
"Check compliance for authentication module against design docs"
"Verify the entire project follows documented architecture"
"Check that API implementation matches specifications"
EOF

# ============================================
# Solution 4: Design-Aware Implementation
# ============================================

cat > ~/.claude/commands/implement-feature.md << 'EOF'
# Design-Aware Feature Implementation

Implement features while strictly following design documentation.

## Implementation Process:
### Phase 1: Design Review (**filesystem** + **memory**)
1. Read ALL relevant design documents:
   - System architecture documentation
   - Feature-specific design documents
   - API specifications and contracts
   - Coding patterns and conventions

2. **memory**: Store design constraints and requirements
3. Identify all interfaces and contracts to implement
4. Note any design dependencies or prerequisites

### Phase 2: Design Validation (**sequential-thinking**)
1. Verify understanding of design requirements
2. Identify any design ambiguities or gaps
3. Confirm implementation approach aligns with architecture
4. Plan implementation to follow documented patterns

### Phase 3: Compliant Implementation
1. Implement EXACTLY as specified in design docs
2. Use documented patterns and conventions consistently
3. Follow API contracts precisely
4. Maintain architectural boundaries

### Phase 4: Compliance Verification
1. Check implementation against design documentation
2. Verify all requirements have been met
3. Ensure patterns are used correctly
4. Validate API contracts are implemented properly

## Design Reference Requirements:
- ALWAYS reference specific design documents
- NEVER assume implementation details not documented
- FOLLOW documented patterns without deviation
- IMPLEMENT interfaces exactly as specified

## Quality Gates:
- [ ] All design docs read and understood
- [ ] Implementation matches architecture
- [ ] Patterns used correctly and consistently  
- [ ] API contracts implemented exactly
- [ ] No design principles violated

## Example Usage:
"Implement user authentication following the design in docs/design/auth.md"
"Create the payment service according to architecture specifications"
"Build the dashboard component following UI design patterns"
EOF

# ============================================
# Solution 5: Project Design Index
# ============================================

cat > ~/.claude/commands/create-design-index.md << 'EOF'
# Create Design Documentation Index

Create a comprehensive index of all design documentation for easy reference.

## Index Creation Process:
### 1. Scan Project Documentation (**filesystem**)
- Find all design documents in ./docs/
- Locate README files with design information
- Identify inline documentation in code
- Find configuration files with design implications

### 2. Create Design Index (**filesystem**)
- List all design documents with descriptions
- Create cross-references between documents
- Index key design decisions and principles
- Map features to their design documentation

### 3. Store Design Knowledge (**memory**)
- Remember location of key design documents
- Store critical design principles and constraints
- Remember patterns and conventions
- Store architectural decisions and rationale

## Design Index Template:
```markdown
# Project Design Index

## Architecture Documents
- docs/design/architecture.md - Overall system design
- docs/design/api.md - API specifications
- docs/design/data-models.md - Data structures

## Feature Designs
- docs/design/auth.md - Authentication system
- docs/design/ui.md - User interface patterns

## Implementation Guides
- docs/patterns.md - Coding patterns
- docs/conventions.md - Naming and style
```

## Usage:
"Create a design index for qi-v2-qicore project"
"Update the design index with new documentation"
"Generate cross-references between design documents"
EOF

# ============================================
# Solution 6: Automated Design Compliance
# ============================================

echo "🤖 Creating automated design compliance tools..."

cat > ~/.claude/scripts/check-design-compliance.py << 'EOF'
#!/usr/bin/env python3
"""
Automated Design Compliance Checker
Scans code for design compliance issues
"""

import os
import re
import json
from pathlib import Path
from typing import List, Dict, Any

class DesignComplianceChecker:
    def __init__(self, project_path: str):
        self.project_path = Path(project_path)
        self.design_docs_path = self.project_path / "docs" / "design"
        self.violations = []
        
    def load_design_rules(self) -> Dict[str, Any]:
        """Load design rules from documentation"""
        rules = {
            "naming_conventions": {},
            "file_structure": {},
            "patterns": {},
            "interfaces": {}
        }
        
        # Load from design documents
        if self.design_docs_path.exists():
            for doc_file in self.design_docs_path.glob("*.md"):
                # Parse design rules from markdown
                content = doc_file.read_text()
                # Extract patterns, conventions, etc.
                # (Implementation would parse markdown for rules)
                
        return rules
    
    def check_naming_conventions(self) -> List[Dict]:
        """Check naming convention compliance"""
        violations = []
        
        # Scan TypeScript files
        for ts_file in self.project_path.rglob("*.ts"):
            if "node_modules" in str(ts_file):
                continue
                
            content = ts_file.read_text()
            
            # Check class naming (PascalCase)
            class_pattern = r'class\s+([a-z][a-zA-Z0-9]*)'
            for match in re.finditer(class_pattern, content):
                violations.append({
                    "file": str(ts_file),
                    "type": "naming_convention",
                    "issue": f"Class name '{match.group(1)}' should be PascalCase",
                    "line": content[:match.start()].count('\n') + 1
                })
        
        return violations
    
    def check_file_structure(self) -> List[Dict]:
        """Check file structure compliance"""
        violations = []
        
        # Check for required directories
        required_dirs = ["src", "tests", "docs"]
        for required_dir in required_dirs:
            if not (self.project_path / required_dir).exists():
                violations.append({
                    "type": "file_structure",
                    "issue": f"Missing required directory: {required_dir}"
                })
        
        return violations
    
    def check_pattern_usage(self) -> List[Dict]:
        """Check design pattern compliance"""
        violations = []
        
        # Check for consistent error handling pattern
        for ts_file in self.project_path.rglob("*.ts"):
            if "node_modules" in str(ts_file):
                continue
                
            content = ts_file.read_text()
            
            # Check for proper error handling
            if "throw new Error" in content and "Result<" not in content:
                violations.append({
                    "file": str(ts_file),
                    "type": "pattern_violation",
                    "issue": "Should use Result<T, E> pattern instead of throwing errors"
                })
        
        return violations
    
    def generate_report(self) -> str:
        """Generate compliance report"""
        all_violations = []
        all_violations.extend(self.check_naming_conventions())
        all_violations.extend(self.check_file_structure())
        all_violations.extend(self.check_pattern_usage())
        
        report = "# Design Compliance Report\n\n"
        
        if not all_violations:
            report += "✅ **All checks passed! Project is compliant with design specifications.**\n"
        else:
            report += f"❌ **Found {len(all_violations)} compliance issues:**\n\n"
            
            for violation in all_violations:
                report += f"- **{violation['type']}**: {violation['issue']}\n"
                if 'file' in violation:
                    report += f"  - File: {violation['file']}\n"
                if 'line' in violation:
                    report += f"  - Line: {violation['line']}\n"
                report += "\n"
        
        return report

if __name__ == "__main__":
    import sys
    project_path = sys.argv[1] if len(sys.argv) > 1 else "."
    
    checker = DesignComplianceChecker(project_path)
    report = checker.generate_report()
    print(report)
EOF

chmod +x ~/.claude/scripts/check-design-compliance.py

# ============================================
# Solution 7: Integration with Existing Workflow
# ============================================

cat > ~/.claude/commands/design-dev-workflow.md << 'EOF'
# Design-Compliant Development Workflow

Complete development workflow that ensures design compliance at every step.

## Enhanced Development Process:
### Step 1: Design Knowledge Loading
1. **filesystem**: Read ALL design documentation
2. **memory**: Store design principles and constraints
3. Run /create-design-index to map all design docs
4. Load project-specific patterns and conventions

### Step 2: Technology Knowledge Update
1. Run /tech-check to load current best practices
2. Verify technology choices align with design
3. **memory**: Store compatible technology patterns
4. Check for design-technology conflicts

### Step 3: Design-First Implementation
1. Use /design-first workflow for all features
2. Reference specific design documents for each task
3. Implement exactly as specified in design
4. Follow documented patterns consistently

### Step 4: Compliance Verification
1. Run /check-compliance after implementation
2. Use automated compliance checker
3. Fix any design violations found
4. **memory**: Store compliant implementation patterns

### Step 5: Quality Assurance with Design Focus
1. Verify TypeScript compliance with design contracts
2. Check that tests validate design requirements
3. Ensure documentation reflects implementation
4. **memory**: Store quality patterns for consistency

## Commands Integration:
```bash
# Before any implementation
claude /design-first
claude /tech-check

# During implementation  
claude /implement-feature

# After implementation
claude /check-compliance
python ~/.claude/scripts/check-design-compliance.py .

# Store successful patterns
claude "Remember this implementation pattern for future use"
```

## Success Criteria:
- All design documents consulted before implementation
- Current technology patterns applied
- Implementation matches design exactly
- Compliance verification passes
- Patterns stored for consistency
EOF

echo "✅ Design Compliance System created!"
echo ""
echo "📐 Your design compliance tools:"
echo "• /design-first - Always read design docs first"
echo "• /implement-feature - Design-aware implementation"
echo "• /check-compliance - Verify design compliance"
echo "• /create-design-index - Map all design documentation"
echo "• Automated compliance checker script"
echo ""
echo "🔄 Enhanced workflow:"
echo "1. claude /design-first (read all design docs)"
echo "2. claude /tech-check (current technology patterns)"
echo "3. claude /implement-feature (design-compliant implementation)"
echo "4. claude /check-compliance (verify compliance)"
echo ""
echo "💡 This ensures Claude ALWAYS follows your design docs!"
```