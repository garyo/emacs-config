# gco-pkm Design Document

## Philosophy

**Goal:** Bring Logseq's workflow fluidity to Emacs without reinventing wheels.

**Approach:** Glue together battle-tested Emacs packages rather than building custom database backend or parser. Focus on **workflow** not **infrastructure**.

## Why Not Database-Backed (Filum)?

Early prototype explored SQLite backend for knowledge management. **Decision: Abandoned** in favor of simpler org-file-based approach.

**Reasons:**
1. **Maintenance burden** - Custom schema, migration, sync with filesystem
2. **Emacs already excellent at text** - org-mode mature, org-ql powerful
3. **Interoperability** - Plain text files work with git, mobile apps, other tools
4. **Trust** - Files don't corrupt; databases can

**Trade-off accepted:** Some query complexity vs database relations, but org-ql is surprisingly powerful.

## Core Architecture

### Components

```
gco-pkm.el
├── Journal system (per-day files in journals/ directory)
├── Page creation and management
├── Query link handlers (clickable [[query:tag]] links)
├── Block references and transclusion
├── Image attachment with auto-optimization
└── Capture templates

gco-pkm-consult.el
├── Interactive search interface
├── Live preview navigation
└── org-ql result transformation

gco-pkm-transient.el
└── Transient menu system (C-c C-/)

gco-pkm-calendar.el
├── Calendar-based journal browsing
├── Date marking for existing journals
└── Preview while navigating

gco-inline-tags.el
├── Inline #tag fontification
├── Click-to-search behavior
└── Tag collection via ripgrep
```

### Package Dependencies

**org-node / org-mem** - Provides:
- Fast file indexing (replaces org-roam)
- Bidirectional linking
- Node management (files + ID-based headings)
- Backlink display
- Completion at point for node titles
- Sequence navigation for daily journals

**org-ql** - Provides:
- SQL-like query DSL for org files
- Boolean logic (AND, OR, NOT)
- Rich predicates (tags, todo, property, date, content)
- Dynamic blocks for auto-updating views

**consult** - Provides:
- Interactive selection with live preview
- Familiar interface (like consult-ripgrep)
- State management for temporary navigation

**org-transclusion** - Provides:
- Block embedding across files
- Content reuse without duplication

### Data Model

**Journal:** Per-day files in `journals/` subdirectory:
```
journals/
├── 2025-10-24.org
├── 2025-10-25.org
└── ...
```

Each file has a top-level heading with active timestamp:
```org
* <2025-10-24 Thu>
Your daily notes here...
```

**Why per-day files?**
- Clean separation, easy to navigate
- Works well with org-node sequence navigation
- Git-friendly (per-file diffs)
- Calendar integration marks dates with existing files

**Topics:** Separate files (Logseq/Roam style)
- One file per major topic/project
- Cross-linked via org-node IDs
- Flexible organization (directories, flat, mixed)

## Key Design Decisions

### 1. org-node Over org-roam

**Problem:** org-roam was slow and had a heavy SQLite dependency.

**Solution:** Switched to org-node/org-mem for faster, simpler node management.

**Benefits:**
- Faster indexing and lookup
- Lighter weight (no external database process)
- Better completion-at-point integration
- Built-in sequence navigation for journals

### 2. Per-Day Journal Files Over Single File

**Previous:** Single `journal.org` with Year → Month → Day hierarchy using org-ml for AST manipulation.

**Current:** One file per day in `journals/` directory.

**Why the change:**
- Simpler file management
- Better git history (per-day diffs)
- Works naturally with org-node sequences
- Calendar integration can mark dates by checking file existence
- No need for complex org-ml tree manipulation

### 3. Consult Integration for Navigation

**Problem:** org-ql provides query results, but navigation was clunky.

**Solution:** Integrate with consult for live preview
- As you navigate results (C-n/C-p), temporarily jumps to location
- See context without committing to the jump
- Familiar interface for Emacs users

### 4. Inline #Tags (gco-inline-tags)

**Problem:** Org tags (`:`-delimited on headings) are limited — they only apply to headings and aren't inline.

**Solution:** Custom inline #tag system:
- Write `#tag` anywhere in body text
- Fontified as clickable links
- RET or click searches for that tag across all notes
- Tag collection via ripgrep with PCRE2 lookarounds
- Complements (not replaces) standard org tags

### 5. Standard Packages Over Custom Code

**Principle:** Use existing solutions when they're 80% there

**Examples:**
- org-node for backlinks (don't reimplement graph)
- org-ql for queries (don't write SQL-like parser)
- consult for navigation (don't build completion UI)
- org-transclusion for block embedding

**Where custom code makes sense:**
- Glue between packages (consult + org-ql integration)
- Workflow-specific commands (journal-today)
- UX improvements (query links, inline tags, calendar browsing)

**Result:** Small codebase, maintainable, leverages ecosystem

## What's Working Well

- Journal system with per-day files and calendar browsing
- Interactive search with live preview (consult + org-ql)
- org-node for fast backlinking and node navigation
- Inline #tags for Logseq-style tagging
- Transient menu for discoverability
- Image attachment with auto-optimization
- Capture templates for journal, notes, TODOs
- Query links (`[[query:tag]]`) for dynamic navigation

## What Needs Work

### Mobile Workflow

**Gap:** No good mobile editing story yet.

**Needs:**
- File sync solution (Syncthing? iCloud?)
- Mobile-optimized capture workflow
- Conflict resolution strategy

### Template System

**Current:** Basic capture templates.

**Desired:** Richer templates for common note types (meeting notes, book notes, project notes).

### Performance at Scale

**Current:** Works well at current knowledge base size.

**Potential concerns:**
- org-ql can be slow on very large knowledge bases
- Inline tag collection scales with file count

## Comparison to Logseq

### Where We Match
- Daily journal workflow
- Bidirectional linking
- Tag-based navigation (#tags + org tags)
- Query system for discovery
- Block-level references (via org-transclusion)

### Where We Differ
- File boundaries more visible in Emacs
- No built-in graph visualization (use org-roam-ui separately)
- Outline-based (org) vs block-based (Logseq)
- Desktop-first (mobile is secondary)

### Where We Excel
- Full power of Emacs (keyboard macros, registers, etc.)
- org-mode features (LaTeX, tables, exports, agenda)
- Extensibility (it's just elisp)
- Plain text files (no database lock-in)
- Decades of org-mode ecosystem

## References

- org-node: https://github.com/meedstrom/org-node
- org-ql: https://github.com/alphapapa/org-ql
- consult: https://github.com/minad/consult
- org-transclusion: https://github.com/nobiot/org-transclusion
- Logseq: https://logseq.com/

## Change Log

- **2025-10:** Initial design document; abandoned Filum database approach
- **2026-02:** Updated to reflect switch from org-roam to org-node, per-day journal files, inline tags, calendar browsing; removed org-ml dependency
