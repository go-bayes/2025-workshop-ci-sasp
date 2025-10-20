# PDF Content Analysis for Figure Mapping

Based on the content analysis of `01-background.qmd`, here's what each figure should contain:

## Current Problematic Mappings

### ✅ `#fig-conventions` (Line 134) - CORRECT
- **Content**: "conventions that describe the meanings of our symbols"
- **Description**: "Our variable naming conventions"  
- **Current PDF**: `1a-terminologylocalconventions.pdf` ✅ CORRECT
- **Variables described**: X, A, Y, Y(a), L, U, M, X̄, R

### ❌ `#fig-general` (Line 176) - WRONG
- **Content**: "conventions that describe components of our causal graphs"
- **Description**: "Nodes, Edges, Conditioning Conventions"
- **Should contain**: Arrow types (black=causality, red=backdoor, dashed=attenuation), boxes (conditioning)
- **Current PDF**: `1a-terminologylocalconventions.pdf` ❌ WRONG
- **Should use**: `S1-graphical-key.pdf` (based on filename and content description)

### ❌ `#fig-symbol-key` (Line 285) - WRONG  
- **Content**: "our conventions" (seems to be about confounding rules)
- **Description**: "Four rules of confounding control"
- **Current PDF**: `1a-terminologylocalconventions.pdf` ❌ WRONG  
- **Should use**: Need to identify correct PDF for confounding control rules

### ❌ `#fig-terminologyconfounders` (Line 302) - WRONG
- **Content**: "four elementary rules of confounding control"
- **Description**: "Four rules of confounding control" 
- **Current PDF**: `1a-terminologylocalconventions.pdf` ❌ WRONG
- **Should use**: Same as above - likely `S2-glossary.pdf` or related confounding PDF

## Available PDFs Analysis

Looking at available PDFs:
- `1a-terminologylocalconventions.pdf` - Variable naming (correctly used for #fig-conventions)
- `1b-terminologydirectedgraph.pdf` - Five elementary structures (correctly used for #fig-directedgraph)
- `S1-graphical-key.pdf` - Likely contains graphical symbols and conventions
- `S2-glossary.pdf` - Likely contains terminology and rules
- Other PDFs focused on specific topics (confounding, measurement error, etc.)

## Proposed Correct Mappings

1. **`#fig-conventions`** → `1a-terminologylocalconventions.pdf` ✅ (already correct)
2. **`#fig-general`** → `S1-graphical-key.pdf` (nodes, edges, conditioning conventions)
3. **`#fig-symbol-key`** → `S1-graphical-key.pdf` or `S2-glossary.pdf` (confounding control conventions)
4. **`#fig-terminologyconfounders`** → `S2-glossary.pdf` (confounding control rules)