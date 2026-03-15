#!/bin/bash

# 1. Parameterize Version (defaults to 'latest' if no argument is passed)
VERSION="${1:-latest}"
RELEASE_DIR="leo-macos-$VERSION"
ARCHIVE_NAME="leo-macos-$VERSION.tar.gz"

# 2. Setup paths
CS_BIN_DIR="deps/ChezScheme/tarm64osx/bin/tarm64osx"
CS_BOOT_DIR="deps/ChezScheme/tarm64osx/boot/tarm64osx"

# 3. Build the engine if missing
if [ ! -f "$CS_BIN_DIR/scheme" ]; then
    echo "ChezScheme binary not found. Building engine..."
    cd deps/ChezScheme && ./configure && make && cd ../..
fi

# 4. Setup dist structure
echo "Preparing dist environment..."
rm -rf dist release
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
mv "dist/lib/leo-whole.so" dist/lib/ 2>/dev/null || true

echo "Copying wrapper from leo/leo to dist/bin/leo..."
cp leo/leo dist/bin/leo
chmod +x dist/bin/leo

# 7. Archive logic with dynamic naming
echo "Creating release archive: release/$ARCHIVE_NAME"
# Create symlink with the versioned name pointing to dist
ln -s dist "$RELEASE_DIR"

# Archive following the symlink
tar -chzf "release/$ARCHIVE_NAME" "$RELEASE_DIR"

# Clean up symlink
rm "$RELEASE_DIR"

echo "Done! Archive created at: release/$ARCHIVE_NAME"
