#!/bin/bash

# Exit immediately if a command exits with a non-zero status.
# -e: exit on error
# -u: exit on unset variables
# -o pipefail: catch errors in pipelines (like find | grep)
set -euo pipefail
shopt -s extglob

# --- 1. Configuration & Versioning ---
VERSION="${1:-latest}"

# Source Directories
SRC_LEO_DIR="leo"
DEPS_DIR="deps/ChezScheme"

# Build/Release Directories
BUILD_DIR="build"
RELEASE_DIR="$BUILD_DIR/release"
REL_BIN_DIR="$RELEASE_DIR/bin"
REL_LIB_DIR="$RELEASE_DIR/lib"
REL_MICASCHEME_DIR="$REL_LIB_DIR/micascheme"

# The "Mini-Prefix" for the Scheme engine
REL_SCHEME_ROOT="$REL_LIB_DIR/scheme"
REL_SCHEME_BIN="$REL_SCHEME_ROOT/bin"
REL_SCHEME_LIB="$REL_SCHEME_ROOT/lib"

REL_EX_DIR="$RELEASE_DIR/examples"

mkdir -p "$REL_BIN_DIR" "$REL_LIB_DIR" "$REL_SCHEME_BIN" "$REL_SCHEME_LIB" "$REL_EX_DIR" "$REL_MICASCHEME_DIR"

# Copy micascheme
EXCLUDE_DIR=$(basename "$REL_MICASCHEME_DIR")
find . -maxdepth 1 ! -path . ! -path "./$EXCLUDE_DIR" -exec cp -R {} "$REL_MICASCHEME_DIR/" \;

# --- 3. Ensure Submodules & Build ChezScheme ---
if [ ! -f "$DEPS_DIR/configure" ]; then
    echo "Submodules missing. Initializing..."
    git submodule update --init --recursive --depth=1
fi

get_machine_type() {
  find . -path "*/bin/*/scheme" 2>/dev/null | \
  grep -v "pb/bin/pb/scheme" | \
  head -n 1 | \
  awk -F'/' '{print $(NF-1)}'
}

if [ -z $(get_machine_type) ]; then
    echo "ChezScheme not found. Building..."
    (cd "$DEPS_DIR" && ./configure && make)
fi

MACHINE=$(get_machine_type)

if [ -z "$MACHINE" ]; then
    echo "ERROR: Machine type could not be detected!"
    ls -R deps/ChezScheme/bin 2>/dev/null
    exit 1
fi

echo "Machine type: $MACHINE"

CS_MACHINE_DIR="$DEPS_DIR/$MACHINE"
CS_BIN_DIR="$CS_MACHINE_DIR/bin/$MACHINE"
CS_BOOT_DIR="$CS_MACHINE_DIR/boot/$MACHINE"

RELEASE_NAME="leo-$MACHINE-$VERSION"
ARCHIVE_NAME="$RELEASE_NAME.tar.gz"

# --- 4. Setup Build/Release Structure ---
echo "Preparing environment for version: $VERSION"
rm -rf "$BUILD_DIR" "$RELEASE_NAME"
mkdir -p "$REL_BIN_DIR" "$REL_LIB_DIR" "$REL_SCHEME_BIN" "$REL_SCHEME_LIB" "$REL_EX_DIR" "$REL_MICASCHEME_DIR"

# Copy micascheme
cp -R * "$REL_MICASCHEME_DIR"
rm -rf "$REL_MICASCHEME_DIR/deps"
rm -rf "$REL_MICASCHEME_DIR/build"

# Copy scheme to its nested bin
cp "$CS_BIN_DIR/scheme" "$REL_SCHEME_BIN/scheme"

# Copy boot files to their nested lib
cp "$CS_BOOT_DIR/petite.boot" "$REL_SCHEME_LIB/"
cp "$CS_BOOT_DIR/scheme.boot" "$REL_SCHEME_LIB/"

# Copy examples
cp "$SRC_LEO_DIR/examples"/* "$REL_EX_DIR/"

# --- 5. Run WPO Compilation ---
echo "Compiling Leo $VERSION with WPO..."
# We run from the new nested bin path
"$REL_SCHEME_BIN/scheme" \
    -b "./$REL_SCHEME_LIB/petite.boot" \
    -b "./$REL_SCHEME_LIB/scheme.boot" \
    --libdirs "$REL_MICASCHEME_DIR" \
    --program "$SRC_LEO_DIR/compile-wpo.ss" \
    "$REL_LIB_DIR/leo.so"

# --- 6. Handle Wrapper Script ---
if [ -f "$SRC_LEO_DIR/leo" ]; then
    echo "Copying wrapper to $REL_BIN_DIR/leo..."
    cp "$SRC_LEO_DIR/leo" "$REL_BIN_DIR/leo"
    chmod +x "$REL_BIN_DIR/leo"
else
    echo "Error: Wrapper script '$SRC_LEO_DIR/leo' not found!"
    exit 1
fi

# --- 7. Archive Logic ---
echo "Creating distribution archive..."
ln -s "$RELEASE_DIR" "$RELEASE_NAME"
tar -chzf "$BUILD_DIR/$ARCHIVE_NAME" "$RELEASE_NAME"
rm "$RELEASE_NAME"

echo "Done! Final release: $BUILD_DIR/$ARCHIVE_NAME"
