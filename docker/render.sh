#!/usr/bin/env bash
set -euo pipefail

# render.sh — usage: render <path-to-org-file> [--css <css-file>]

FILE=""
CSS_NAME=""

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --css)
      shift
      CSS_NAME="$1"
      shift
      ;;
    *)
      FILE="$1"
      shift
      ;;
  esac
done

# Validate input file
if [[ -z "$FILE" || ! -f "$FILE" ]]; then
  echo "Usage: render <path-to-org-file> [--css <css-file>]" >&2
  exit 1
fi

# Determine output filename and clean backups
BASE="${FILE%.*}"
OUT_HTML="${BASE}.html"
rm -f "${OUT_HTML}~"

# Build Emacs batch command as array
EMACS_CMD=(emacs --batch -q --load /root/.emacs.d/init.el)

# Inline CSS if requested
if [[ -n "$CSS_NAME" ]]; then
  CSS_PATH="/usr/local/share/org-css/${CSS_NAME}.css"
  if [[ -f "$CSS_PATH" ]]; then
    echo "INFO: Inlining CSS from '$CSS_PATH'"
    EMACS_CMD+=(--eval "(with-temp-buffer (insert-file-contents \"$CSS_PATH\") (setq org-html-head-extra (concat \"<style type=\\\"text/css\\\">\" (buffer-string) \"</style>\")))")
  else
    echo "WARNING: CSS file '$CSS_PATH' not found, proceeding without additional CSS" >&2
  fi
fi

# Add export invocation
EMACS_CMD+=("$FILE" -l org --eval "(org-html-export-to-html)")

# Execute Emacs command
"${EMACS_CMD[@]}"

# Cleanup any HTML backup file
rm -f "${OUT_HTML}~"

# Restore ownership to match input .org
OWNER_UID=$(stat -c '%u' "$FILE")
OWNER_GID=$(stat -c '%g' "$FILE")
chown "$OWNER_UID:$OWNER_GID" "$OUT_HTML"

echo "Generated: /workspace/$OUT_HTML"
