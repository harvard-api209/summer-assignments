#!/usr/bin/env bash
# One-command verification for the student site. Run before any push.
set -euo pipefail
cd "$(dirname "$0")/.."

echo "— JS syntax"
node --check docs/site.js
node --check docs/assignment-player.js
node --check docs/webr-runner.js

echo "— player round-trip tests"
node --test tests/*.test.mjs

echo "— internal links and anchors"
python3 tools/check_links.py --strict

echo "— generated web assignments in sync with canonical assignments"
python3 tools/generate_web_assignments.py --check

echo "ALL CHECKS PASSED"
