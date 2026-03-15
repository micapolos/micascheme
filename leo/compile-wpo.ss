(import (chezscheme))

(optimize-level 3)
(compile-imported-libraries #t)
(generate-wpo-files #t)

(display "Compiling program...")
(newline)
(compile-program "leo/main.ss" "build/release/lib/leo.so")

(display "Compiling whole program...")
(newline)
(compile-whole-program "build/release/lib/leo.wpo" "build/release/lib/leo-wpo.so" #t)
