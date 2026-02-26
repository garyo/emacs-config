# gco-pkm - Personal Knowledge Management for Emacs

A Logseq-inspired Personal Knowledge Management system built on standard Emacs packages.

## What It Is

gco-pkm provides a unified, searchable knowledge base in Emacs with:
- Daily journal files with hierarchical navigation
- Topic-based notes with bidirectional linking (via org-node)
- Powerful query interface across your entire knowledge base (via org-ql)
- Live preview navigation with consult
- Inline #hashtag support and search
- Calendar-based journal browsing
- Transient menu for quick access to all commands

**Goal:** Bring Logseq's workflow fluidity to Emacs while leveraging org-mode's power.

## Components

```
gco-pkm.el              - Core: journal, pages, queries, capture, block refs
gco-pkm-consult.el      - Consult integration for live preview search
gco-pkm-transient.el    - Transient menu interface (C-c C-/)
gco-pkm-calendar.el     - Calendar-based journal browsing
gco-inline-tags.el      - Inline #tag fontification and search
```

## Dependencies

### Required Packages
- **org-mode** (built-in) - Document structure
- **org-node** / **org-mem** - Node management, backlinking, fast indexing
- **org-ql** - Query DSL for searching org files
- **consult** - Interactive navigation with live preview
- **org-transclusion** - Block embedding across files

### Optional but Recommended
- **ripgrep** (`rg`) - Fast text/tag search
- **magick** or **sips** - Image optimization on attach

## File Structure

Your knowledge base lives in `~/Documents/org-notes/` (or your configured directory):

```
org-notes/
├── journal.org         # Legacy single-file journal (used for some captures)
├── journals/           # Daily journal files (YYYY-MM-DD.org)
│   ├── 2025-10-24.org
│   ├── 2025-10-25.org
│   └── ...
├── topics/             # Topic-based notes
├── projects/           # Project-specific notes
└── ...                 # Other org files
```

## Key Bindings

| Key         | Command                        | Description                    |
|-------------|--------------------------------|--------------------------------|
| `C-c C-/`   | `gco-pkm-menu`               | Main transient menu            |
| `C-c j`     | `gco-pkm-journal-today`      | Open today's journal           |
| `C-c c`     | `org-capture`                 | Capture (journal/note/TODO)    |
| `C-c n f`   | `org-node-find`               | Find/create page by title      |
| `C-c n i`   | `org-node-insert-link`        | Insert node link               |
| `C-c n s`   | `org-node-grep`               | Full-text search across nodes  |
| `C-c n b`   | `org-node-context-dwim`       | Show backlinks                 |
| `C-c t s`   | `gco-inline-tags-search`      | Search inline #tags            |
| `C-c t i`   | `gco-inline-tags-insert`      | Insert inline #tag             |

## Workflow

**Morning:**
1. `C-c j` — Open today's journal
2. Write notes, add #hashtags and [[links]] as you go

**Throughout the day:**
- `C-c c` to capture thoughts, notes, TODOs
- Create topic files as needed (`C-c n f`)
- Cross-link related ideas (`C-c n i`)

**Discovery:**
- `C-c n s` — Grep across all notes
- `C-c t s` — Search by #tag
- `C-c n b` — See backlinks to current node
- Click `[[query:tag]]` links for filtered org-ql views
- `C-c C-/` — Transient menu for everything else

## Capture Templates

- `j` (Journal) — Capture to today's journal file
- `n` (Note) — Capture to journal.org with timestamp
- `t` (TODO) — Capture to journal.org under "Tasks"
- `f` (New File Note) — Create new note file
- `i` (ID Node) — Capture to new org-node with ID

## Documentation

See `gco-pkm-DESIGN.md` for architecture decisions and design rationale.
