#!/bin/bash

# 1. Parameterize Version
VERSION="${1:-latest}"
RELEASE_DIR_NAME="leo-macos-$VERSION"
ARCHIVE_NAME="leo-macos-$VERSION.tar.gz"

# 2. Ensure submodules are present
# This checks if the ChezScheme directory is empty or missing
if [ ! -f "deps/ChezScheme/configure" ]; then
    echo "Submodules missing. Initializing..."
    git submodule update --init --recursive
fi

# 3. Setup paths
CS_BIN_DIR="deps/ChezScheme/tarm64osx/bin/tarm64osx"
CS_BOOT_DIR="deps/ChezScheme/tarm64osx/boot/tarm64osx"

# 4. Build ChezScheme if missing
if [ ! -f "$CS_BIN_DIR/scheme" ]; then
    echo "ChezScheme not found. Building..."
    cd deps/ChezScheme
    ./configure
    make
    cd ../..
fi

# 5. Setup build/release structure
echo "Preparing environment for version: $VERSION"
rm -rf build "$RELEASE_DIR_NAME"
mkdir -p build/release/bin build/release/lib

cp "$CS_BIN_DIR/scheme" build/release/bin/scheme
cp "$CS_BOOT_DIR/petite.boot" build/release/lib/
cp "$CS_BOOT_DIR/scheme.boot" build/release/lib/

# 6. Run WPO compilation
echo "Compiling Leo $VERSION with WPO..."
./build/release/bin/scheme \
    -b ./build/release/lib/petite.boot \
    -b ./build/release/lib/scheme.boot \
    --program "leo/compile-wpo.ss"

if [ -f "leo/leo" ]; then
    echo "Copying wrapper to build/release/bin/leo..."
    cp leo/leo build/release/bin/leo
    chmod +x build/release/bin/leo
else
    echo "Error: Wrapper script 'leo/leo' not found!"
    exit 1
fi

# 7. Archive logic
echo "Creating distribution archive..."

# Create symlink so the folder name inside the tar is leo-macos-$VERSION
ln -s build/release "$RELEASE_DIR_NAME"

# Archive the symlink (dereferenced) into the build folder
tar -chzf "build/$ARCHIVE_NAME" "$RELEASE_DIR_NAME"

# Clean up the temporary symlink
rm "$RELEASE_DIR_NAME"

if [ -f "build/$ARCHIVE_NAME" ]; then
    echo "Successfully created: build/$ARCHIVE_NAME"
else
    echo "Archive creation failed!"
    exit 1
fi

echo "Done! Final release is in 'build/release' and archive is in 'build/'."
