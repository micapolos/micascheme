(library (micac run)
  (export micac-run)
  (import (micascheme) (c run) (micac c) (micac syntax))
  (export (import (micac syntax) (micac c)))

  (define-rule-syntax (micac-run instr ...)
    (lets
      ($string (micac-c (begin instr ...)))
      ($string
        (lines-string
          "#include <stdlib.h>"
          "#include <stdio.h>"
          "#include <stdbool.h>"
          "#include <SDL.h>"
          ""
          (string-append "int main() " $string)))
      (c-run $string "`sdl2-config --cflags --libs`")))
)
