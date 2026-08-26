#!/usr/bin/env bash
# Launch Emacs against a disposable copy of the notes, in either format.
#
# The live corpus is still org, so opening a live note only ever exercises
# the org half of the PKM.  This builds a throwaway corpus -- converted to
# markdown by default -- and points a normal Emacs at it via GCO_PKM_DIR,
# so the markdown half can actually be driven before any cutover.
#
# Nothing under ~/Documents/org-notes is read-write here: the source is
# copied, and Syncthing never sees the sandbox.
#
#   ./pkm-sandbox.sh            # markdown sandbox (default)
#   ./pkm-sandbox.sh org        # org sandbox, for an A/B comparison
#   ./pkm-sandbox.sh md ~/tmp/x # choose where it lives
#
set -euo pipefail

FORMAT="${1:-md}"
SANDBOX="${2:-${TMPDIR:-/tmp}/pkm-sandbox-$FORMAT}"
LIVE="$HOME/Documents/org-notes"
REPO="$HOME/src/gco-pkm-llm"

if [ "$FORMAT" != "md" ] && [ "$FORMAT" != "org" ]; then
    echo "usage: $0 [md|org] [sandbox-dir]" >&2
    exit 2
fi

echo "==> Building $FORMAT sandbox at $SANDBOX"
rm -rf "$SANDBOX"

if [ "$FORMAT" = "org" ]; then
    cp -a "$LIVE" "$SANDBOX"
    rm -rf "$SANDBOX/.git" "$SANDBOX/.stfolder"
else
    SRC="$SANDBOX.src"
    rm -rf "$SRC"
    cp -a "$LIVE" "$SRC"
    rm -rf "$SRC/.git" "$SRC/.stfolder"
    ( cd "$REPO" && uv run python scripts/org2md.py "$SRC" "$SANDBOX" ) | tail -5
    ( cd "$REPO" && uv run python scripts/verify_org2md.py "$SRC" "$SANDBOX" ) | head -6
    rm -rf "$SRC"
fi

cat <<EOF

==> Sandbox ready: $SANDBOX
    $(find "$SANDBOX" -name '*.md' -o -name '*.org' | wc -l | tr -d ' ') notes, $(find "$SANDBOX/assets" -type f 2>/dev/null | wc -l | tr -d ' ') assets

Try, in the Emacs that opens:
    C-c C-/           the PKM menu -- check it looks right for $FORMAT
    jj                today's journal (should be created as .$FORMAT)
    sp                find a page by title
    sd                open TODOs
    nc                context sidebar (open a note first)
    C-c C-y           paste an image (should land in assets/, optimized)
    cb                block reference (org: id link; md: ^anchor)

Anything you break in here is disposable. Delete with:
    rm -rf "$SANDBOX"
EOF

GCO_PKM_DIR="$SANDBOX" exec emacs
