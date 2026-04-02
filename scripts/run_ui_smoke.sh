#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

echo "== Frontend Unit Tests =="
node --test frontend/tests/*.test.js

echo
echo "== Frontend Build =="
if [[ -x frontend/node_modules/.bin/vite ]]; then
  npm --prefix frontend run build
else
  echo "Skipping frontend build: frontend/node_modules/.bin/vite is not installed in this checkout."
fi

echo
echo "== App Lane =="
raco test racket-server/tests/test-app.rkt

echo
echo "== Visible Contract Lane =="
raco test racket-server/tests/visible-contract-tests.rkt

echo
echo "== Frontend State Smoke =="
node frontend/tests/ui-state-smoke.mjs

echo
echo "== Backend Payload Smoke =="
racket racket-server/tests/ui-payload-smoke.rkt
