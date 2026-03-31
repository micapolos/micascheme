(import (chezscheme))

(suppress-greeting #t)
(compile-imported-libraries #t)
(generate-wpo-files #t)
(optimize-level 3)
(generate-inspector-information #f)

(display "Compiling program...")
(newline)
(compile-program "leo/main.ss" "leo/main.so")

(display "Compiling whole program...")
(newline)
(compile-whole-program "leo/main.wpo" "leo/leo.so" #t)

(display "Making boot file...")
(newline)
(make-boot-file
  (car (command-line-arguments))
  (list "scheme" "petite")
  "leo/leo.so")
