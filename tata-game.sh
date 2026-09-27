#!/bin/zsh

set -euo pipefail

scheme --program tata-8/compile.ss tata-8/game.leo ~/git/Tata8/src/micapolos/zexy/examples/Leo.kt
(
  cd ~/git/Tata8 || exit 1
  kotlinc $(find src -name "*.java" -o -name "*.kt") -d out/built
  javac -cp out/built -d out/built $(find src -name "*.java")
  cp -R res/. out/built/
  kotlin -cp "out/built" micapolos.zexy.examples.LeoKt
)
