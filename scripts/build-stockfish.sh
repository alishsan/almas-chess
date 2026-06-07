#!/usr/bin/env bash
# Build patched Stockfish with validmoves/simulate commands for almas-chess.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TAG="${STOCKFISH_TAG:-sf_12}"
DIR="${STOCKFISH_DIR:-$ROOT/vendor/stockfish}"
ARCH="${ARCH:-}"
if [[ -z "$ARCH" ]]; then
  case "$(uname -m)" in
    arm64|aarch64) ARCH=apple-silicon ;;
    *) ARCH=x86-64 ;;
  esac
fi

if [[ ! -d "$DIR/.git" ]]; then
  echo "Cloning Stockfish ($TAG) into $DIR ..."
  git clone --depth 1 --branch "$TAG" https://github.com/official-stockfish/Stockfish.git "$DIR"
fi

echo "Patching Stockfish uci.cpp (validmoves command) ..."
git -C "$DIR" checkout -- src/uci.cpp
patch -p1 -d "$DIR" < "$ROOT/scripts/stockfish-validmoves.patch"

echo "Building ..."
make -C "$DIR/src" build ARCH="$ARCH"

BIN="$DIR/src/stockfish"
if [[ ! -x "$BIN" ]]; then
  echo "Build failed: $BIN not found" >&2
  exit 1
fi

echo ""
echo "Built: $BIN"
echo "Use with:"
echo "  export ALMAS_STOCKFISH=$BIN"
