#!/bin/bash

# 1. Setup paths
CS_BIN_DIR="deps/ChezScheme/tarm64osx/bin/tarm64osx"
CS_BOOT_DIR="deps/ChezScheme/tarm64osx/boot/tarm64osx"

# 2. Build the engine if missing
if [ ! -f "$CS_BIN_DIR/scheme" ]; then
    echo "ChezScheme binary not found. Building engine..."
    cd deps/ChezScheme && ./configure && make && cd ../..
fi

# 3. Setup dist structure
echo "Preparing dist environment..."
mkdir -p dist/bin dist/lib
cp "$CS_BIN_DIR/scheme" dist/bin/scheme
cp "$CS_BOOT_DIR/petite.boot" dist/lib/
cp "$CS_BOOT_DIR/scheme.boot" dist/lib/

# 4. Run WPO compilation
echo "Compiling Leo with WPO..."
./dist/bin/scheme \
    -b ./dist/lib/petite.boot \
    -b ./dist/lib/scheme.boot \
    --program "leo/compile-wpo.ss"

# 5. Move output and copy your pre-written wrapper
mv "dist/lib/leo-whole.so" dist/lib/ 2>/dev/null || true

echo "Copying wrapper from leo/leo.sh to dist/bin/leo..."
cp leo/leo dist/bin/leo
chmod +x dist/bin/leo

echo "Done! Run with: ./dist/bin/leo"
