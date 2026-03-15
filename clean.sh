#!/bin/bash
rm -rf build
find . -type f \( -name "*.wpo" -o -name "*.so" \) -delete
