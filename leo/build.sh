#!/bin/bash

# --- 1. Configuration & Versioning ---
VERSION="${1:-latest}"
MACHINE="tarm64osx"

# Source Directories
SRC_LEO_DIR="leo"
DEPS_DIR="deps/ChezScheme"
CS_MACHINE_DIR="$DEPS_DIR/$MACHINE"

# Build/Release Directories
BUILD_DIR="build"
RELEASE_DIR="$BUILD_DIR/release"
REL_BIN_DIR="$RELEASE_DIR/bin"
REL_LIB_DIR="$RELEASE_DIR/lib"
REL_EX_DIR="$RELEASE_DIR/examples"

# Artifact Names
RELEASE_NAME="leo-macos-$VERSION"
ARCHIVE_NAME="$RELEASE_NAME.tar.gz"

# --- 2. Internal ChezScheme Paths ---
CS_BIN_DIR="$CS_MACHINE_DIR/bin/$MACHINE"
CS_BOOT_DIR="$CS_MACHINE_DIR/boot/$MACHINE"

# --- 3. Ensure Submodules & Build ChezScheme ---
if [ ! -f "$DEPS_DIR/configure" ]; then
    echo "Submodules missing. Initializing..."
    git submodule update --init --recursive --depth=1
fi

if [ ! -f "$CS_BIN_DIR/scheme" ]; then
    echo "ChezScheme not found. Building..."
    (cd "$DEPS_DIR" && ./configure && make)
fi

# --- 4. Setup Build/Release Structure ---
echo "Preparing environment for version: $VERSION"
rm -rf "$BUILD_DIR" "$RELEASE_NAME"
mkdir -p "$REL_BIN_DIR" "$REL_LIB_DIR" "$REL_EX_DIR"

# Copy engine and boot files
cp "$CS_BIN_DIR/scheme" "$REL_BIN_DIR/scheme"
cp "$CS_BOOT_DIR/petite.boot" "$REL_LIB_DIR/"
cp "$CS_BOOT_DIR/scheme.boot" "$REL_LIB_DIR/"

# Copy examples
cp "$SRC_LEO_DIR/examples"/* "$REL_EX_DIR/"

# --- 5. Run WPO Compilation ---
echo "Compiling Leo $VERSION with WPO..."
"$REL_BIN_DIR/scheme" \
    -b "./$REL_LIB_DIR/petite.boot" \
    -b "./$REL_LIB_DIR/scheme.boot" \
    --program "$SRC_LEO_DIR/compile-wpo.ss" "$REL_LIB_DIR/leo.so"

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

# Create symlink so the folder name inside the tar is leo-macos-$VERSION
ln -s "$RELEASE_DIR" "$RELEASE_NAME"

# Archive the symlink (dereferenced) into the build folder
tar -chzf "$BUILD_DIR/$ARCHIVE_NAME" "$RELEASE_NAME"

# Clean up the temporary symlink
rm "$RELEASE_NAME"

if [ -f "$BUILD_DIR/$ARCHIVE_NAME" ]; then
    echo "Successfully created: $BUILD_DIR/$ARCHIVE_NAME"
else
    echo "Archive creation failed!"
    exit 1
fi

echo "Done! Final release is in '$RELEASE_DIR' and archive is in '$BUILD_DIR/'."
