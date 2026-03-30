(import (chezscheme))

(compile-imported-libraries #t)
(generate-wpo-files #t)

(display "Compiling program...")
(newline)
(compile-program "leo/main.ss" "leo/main.so")

(display "Compiling libraries...")
(newline)
(compile-library "ansi-string.ss")

(display "Compiling whole program...")
(newline)
(compile-whole-program "leo/main.wpo" (car (command-line-arguments)) #t)
