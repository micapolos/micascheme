#!/bin/bash

# 1. Setup directories
echo "Creating dist structure..."
mkdir -p dist/bin dist/lib

# 2. Build the engine if not already built
# We check if the binary exists to save time
BINARY_PATH=$(find deps/ChezScheme -name scheme -type f -perm +111 | head -n 1)

if [ -z "$BINARY_PATH" ]; then
    echo "ChezScheme not found. Building engine (this will take a while)..."
    cd deps/ChezScheme
    ./configure
    make
    cd ../..
    # Re-search for the binary now that it's built
    BINARY_PATH=$(find deps/ChezScheme -name scheme -type f -perm +111 | head -n 1)
fi

# 3. Locate Boot Files
PETITE_PATH=$(find deps/ChezScheme -name petite.boot | head -n 1)
SCHEME_PATH=$(find deps/ChezScheme -name scheme.boot | head -n 1)

# 4. Copy files to dist
echo "Packaging binaries..."
cp "$BINARY_PATH" dist/bin/leo-bin
cp "$PETITE_PATH" dist/lib/
cp "$SCHEME_PATH" dist/lib/

# 5. Create the 'leo' wrapper
echo "Creating wrapper script..."
echo '#!/bin/bash
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec "$DIR/bin/leo-bin" -b "$DIR/lib/petite.boot" -b "$DIR/lib/scheme.boot" "$@"' > dist/leo

# 6. Make it executable
chmod +x dist/leo

echo "Done! You can now run your local build with: ./dist/leo"
