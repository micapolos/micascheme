(library (micac run)
  (export micac-run)
  (import (micascheme) (c run) (micac c) (micac syntax))
  (export (import (micac syntax)))

  (define-rule-syntax (micac-run instr ...)
    (lets
      ($string (micac-c (begin instr ...)))
      ($string
        (lines-string
          "#include <stdlib.h>"
          "#include <stdio.h>"
          (string-append "int main() " $string)))
      (c-run $string)))
)
