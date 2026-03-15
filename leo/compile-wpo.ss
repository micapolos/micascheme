(import (chezscheme))

(optimize-level 3)
(generate-wpo-files #t)
(compile-program "leo/main.ss" "dist/lib/leo.so")
