#!/bin/bash

# 1. Setup paths
CS_BIN_DIR="deps/ChezScheme/tarm64osx/bin/tarm64osx"
CS_BOOT_DIR="deps/ChezScheme/tarm64osx/boot/tarm64osx"

# 2. Build the engine if missing
if [ ! -f "$CS_BIN_DIR/scheme" ]; then
    echo "ChezScheme binary not found. Building engine..."
    cd deps/ChezScheme && ./configure && make && cd ../..
fi

# 3. Setup dist structure and copy engine components FIRST
echo "Preparing dist environment..."
mkdir -p dist/bin dist/lib
cp "$CS_BIN_DIR/scheme" dist/bin/scheme
cp "$CS_BOOT_DIR/petite.boot" dist/lib/
cp "$CS_BOOT_DIR/scheme.boot" dist/lib/

# 4. Run WPO compilation using the 'dist' binaries
echo "Compiling Leo with WPO using dist binaries..."
./dist/bin/scheme \
    -b ./dist/lib/petite.boot \
    -b ./dist/lib/scheme.boot \
    --program "leo/compile-wpo.ss"

# 5. Move output to dist/lib and create wrapper
mv ".dist/lib/leo-whole.so" dist/lib/

echo "Creating wrapper script..."
echo '#!/bin/bash
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec "$DIR/bin/scheme" -b "$DIR/lib/petite.boot" -b "$DIR/lib/scheme.boot" --program "$DIR/lib/leo-whole.so" "$@"' > dist/leo

chmod +x dist/leo
echo "Done! Run with: ./dist/leo"
