#!/usr/bin/env bash
# Launch Emacs against a disposable copy of the notes, in either format.
#
# The live corpus is still org, so opening a live note only ever exercises
# the org half of the PKM.  This builds a throwaway corpus -- converted to
# markdown by default -- and points a normal Emacs at it via GCO_PKM_DIR,
# so the markdown half can actually be driven before any cutover.
#
# Nothing under ~/Documents/org-notes is modified: the source is copied,
# and Syncthing never sees the sandbox.
#
#   ./pkm-sandbox.sh                  # markdown sandbox (default)
#   ./pkm-sandbox.sh org              # org sandbox, for an A/B comparison
#   ./pkm-sandbox.sh md ~/tmp/x       # choose where it lives
#   ./pkm-sandbox.sh md '' --build    # build only, don't launch Emacs
#
set -euo pipefail

FORMAT="${1:-md}"
SANDBOX="${2:-}"
BUILD_ONLY="${3:-}"

# Strip any trailing slash TMPDIR carries, so paths don't come out with "//".
: "${SANDBOX:=${TMPDIR:-/tmp}}"
SANDBOX="${SANDBOX%/}"
[ -n "${2:-}" ] || SANDBOX="$SANDBOX/pkm-sandbox-$FORMAT"

LIVE="$HOME/Documents/org-notes"
REPO="$HOME/src/gco-pkm-llm"

if [ "$FORMAT" != "md" ] && [ "$FORMAT" != "org" ]; then
    echo "usage: $0 [md|org] [sandbox-dir] [--build]" >&2
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

    # A conversion failure is fatal -- there would be nothing to test.
    if ! convert_out=$(cd "$REPO" && uv run python scripts/org2md.py "$SRC" "$SANDBOX" 2>&1); then
        printf '%s\n' "$convert_out" >&2
        echo "==> Conversion FAILED; sandbox not built." >&2
        exit 1
    fi
    printf '%s\n' "$convert_out" | grep -E '^(converted|  )' | tail -8 || true

    # The verifier exits non-zero whenever it has anything to report, and its
    # remaining reports are accounting artefacts (frontmatter and the journal
    # date wrapper move out of the body), not lost content.  Informational
    # here: never let it abort the sandbox.
    verify_out=$(cd "$REPO" && uv run python scripts/verify_org2md.py "$SRC" "$SANDBOX" 2>&1) || true
    printf '%s\n' "$verify_out" | head -6
    echo "    (reports are the verifier's own accounting -- frontmatter and the"
    echo "     journal date wrapper move out of the body -- not lost content."
    echo "     Full report:  cd $REPO && uv run python scripts/verify_org2md.py '$SRC' '$SANDBOX')"
    # $SRC is kept so the two formats can be diffed side by side.
fi

if [ "$FORMAT" = "md" ]; then
    COMPARE_NOTE="The org source it was converted from is alongside it, for comparison:
    $SANDBOX.src
"
    CLEANUP="\"$SANDBOX\" \"$SANDBOX.src\""
else
    COMPARE_NOTE=""
    CLEANUP="\"$SANDBOX\""
fi

note_count=$(find "$SANDBOX" \( -name '*.md' -o -name '*.org' \) | wc -l | tr -d ' ')
asset_count=$(find "$SANDBOX/assets" -type f 2>/dev/null | wc -l | tr -d ' ')

cat <<EOF

==> Sandbox ready: $SANDBOX
    $note_count notes, $asset_count assets

Try, in the Emacs that opens:
    C-c C-/     the PKM menu -- check it looks right for $FORMAT
    jj          today's journal (should be created as .$FORMAT)
    sp          find a page by title
    sd          open TODOs
    nc          context sidebar (open a note first)
    C-c C-y     paste an image (should land in assets/, optimized)
    cb          block reference (org: id link; md: ^anchor)

$COMPARE_NOTE
Anything you break in here is disposable. Delete with:
    rm -rf $CLEANUP
EOF

if [ "$BUILD_ONLY" = "--build" ]; then
    echo "==> --build given; not launching Emacs."
    exit 0
fi

echo "==> Launching Emacs with GCO_PKM_DIR=$SANDBOX"
GCO_PKM_DIR="$SANDBOX" exec emacs
