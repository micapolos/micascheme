#!/bin/bash

# 1. Parameterize Version
VERSION="${1:-latest}"
RELEASE_DIR="leo-macos-$VERSION"
ARCHIVE_NAME="leo-macos-$VERSION.tar.gz"

# 2. Setup paths
CS_BIN_DIR="deps/ChezScheme/tarm64osx/bin/tarm64osx"
CS_BOOT_DIR="deps/ChezScheme/tarm64osx/boot/tarm64osx"

# 3. Build the engine if missing
if [ ! -f "$CS_BIN_DIR/scheme" ]; then
    echo "ChezScheme binary not found. Building engine..."
    # Ensure we are in the right spot to build
    cd deps/ChezScheme && ./configure && make && cd ../..
fi

# 4. Setup dist and release structure
echo "Preparing environment for version: $VERSION"
# Clean previous builds to prevent stale files in the archive
rm -rf dist release "$RELEASE_DIR"
mkdir -p dist/bin dist/lib release

cp "$CS_BIN_DIR/scheme" dist/bin/scheme
cp "$CS_BOOT_DIR/petite.boot" dist/lib/
cp "$CS_BOOT_DIR/scheme.boot" dist/lib/

# 5. Run WPO compilation
echo "Compiling Leo $VERSION with WPO..."
./dist/bin/scheme \
    -b ./dist/lib/petite.boot \
    -b ./dist/lib/scheme.boot \
    --program "leo/compile-wpo.ss"

# 6. Finalize structure
# Ensure the WPO output is in lib/
mv "dist/lib/leo-whole.so" dist/lib/ 2>/dev/null || true

# Verify wrapper exists before copying
if [ -f "leo/leo" ]; then
    echo "Copying wrapper to dist/bin/leo..."
    cp leo/leo dist/bin/leo
    chmod +x dist/bin/leo
else
    echo "Error: Wrapper script 'leo/leo' not found!"
    exit 1
fi

# 7. Archive logic with dynamic naming
echo "Creating release archive: release/$ARCHIVE_NAME"

# Create symlink so 'tar' sees the folder name as leo-macos-$VERSION
ln -s dist "$RELEASE_DIR"

# -h (dereference) ensures tar archives the folder the symlink points to
tar -chzf "release/$ARCHIVE_NAME" "$RELEASE_DIR"

# Clean up the temporary symlink
rm "$RELEASE_DIR"

if [ -f "release/$ARCHIVE_NAME" ]; then
    echo "Done!"
else
    echo "Archive creation failed!"
    exit 1
fi
