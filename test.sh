#!/bin/bash

set -euo pipefail

echo "Running tests..."
scheme --program all-test.ss

echo "Running leo tests..."
leo-dev leo/all-test.leo

echo "Building Leo..."
./build.sh

echo "Running Leo..."
./build/release/bin/leo -v

echo "All tests passed."
