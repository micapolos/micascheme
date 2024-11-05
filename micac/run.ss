(library (micac run)
  (export micac-run micac-string)
  (import (micascheme) (c run) (micac c) (micac syntax))
  (export (import (micac syntax) (micac c)))

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
    (c-run
      (micac-string instr ...)
      "`sdl2-config --cflags --libs`"))
)
