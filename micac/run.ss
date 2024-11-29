(library (micac run)
  (export micac-run micac-string micac-run-echo?)
  (import (micascheme) (c run) (micac c) (micac keywords))
  (export (import (micac keywords) (micac c)))

  (define micac-run-echo? (make-parameter #f))

  (define-rule-syntax (micac-string instr ...)
    (lets
      ($string (micac-c (begin instr ...)))
      (lines-string
        "#include <stdlib.h>"
        "#include <stdio.h>"
        "#include <stdbool.h>"
        "#include <SDL.h>"
        ""
        (string-append "int main() " $string))))

  (define-rule-syntax (micac-run instr ...)
    (lets
      ($string (micac-string instr ...))
      (run
        (when (micac-run-echo?)
          (call-with-output-file "micac/run.c"
            (lambda ($port) (put-string $port $string))
            `(replace)))
        (c-run $string "`sdl2-config --cflags --libs`" "-Wno-shift-op-parentheses"))))
)
