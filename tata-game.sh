#!/bin/zsh

set -euo pipefail

echo "Translating Leonardo to Kotlin..."
scheme --program tata-8/compile.ss tata-8/game.leo ~/git/Tata8/src/micapolos/zexy/examples/Game.kt
(
  cd ~/git/Tata8 || exit 1
  echo "Compiling Kotlin..."
  kotlinc $(find src -name "*.java" -o -name "*.kt") -d out/built
  echo "Compiling Java..."
  javac -cp out/built -d out/built $(find src -name "*.java")
  echo "Copying resources..."
  cp -R res/. out/built/
  echo "Starting game..."
  kotlin -cp "out/built" micapolos.zexy.examples.GameKt
)
rm ~/git/Tata8/src/micapolos/zexy/examples/Game.kt
echo "Done."
