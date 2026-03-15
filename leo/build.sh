#!/bin/bash

# 1. Parameterize Version
VERSION="${1:-latest}"
RELEASE_DIR_NAME="leo-macos-$VERSION"
ARCHIVE_NAME="leo-macos-$VERSION.tar.gz"

# 2. Ensure submodules are present
# This checks if the ChezScheme directory is empty or missing
if [ ! -f "deps/ChezScheme/configure" ]; then
    echo "Submodules missing. Initializing..."
    git submodule update --init --recursive --depth=1
fi

# 3. Setup paths
CS_BIN_DIR="deps/ChezScheme/tarm64osx/bin/tarm64osx"
CS_BOOT_DIR="deps/ChezScheme/tarm64osx/boot/tarm64osx"
CS_C_DIR="deps/ChezScheme/tarm64osx/c"
CS_LZ4_OBJ_DIR="deps/ChezScheme/tarm64osx/lz4/lib"
CS_ZLIB_OBJ_DIR="deps/ChezScheme/tarm64osx/zlib"
LIB_DIR="build/release/lib"

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

# 6. Run WPO compilation, it puts leo-wpo.so and leo-load.so in build/release/lib
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

# 7. Convert Binaries to C Headers (The missing link)
echo "Embedding binaries into C headers..."
(cd "$CS_BOOT_DIR" && xxd -i "petite.boot") > build/petite_boot.h
(cd "$CS_BOOT_DIR" && xxd -i "scheme.boot") > build/scheme_boot.h
(cd "$LIB_DIR" && xxd -i "leo-wpo.so") > build/leo_wpo_so.h
(cd "$LIB_DIR" && xxd -i "leo-load.so") > build/leo_load_so.h

ZLIB_OBJS=$(ls "$CS_ZLIB_OBJ_DIR"/*.o | grep -vE "example.o|minigzip.o")
LZ4_OBJS="$CS_LZ4_OBJ_DIR"/*.o

clang -O3 \
    -I"$CS_BOOT_DIR" \
    -I./build \
    leo/main.c \
    "$CS_C_DIR"/*.o \
    $LZ4_OBJS \
    $ZLIB_OBJS \
    -liconv -lm -lncurses \
    -o build/release/bin/leoc

# 9. Optimization
strip build/release/bin/leoc

echo "Standalone build complete: build/release/bin/leoc"

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
