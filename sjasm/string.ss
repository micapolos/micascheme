(library (sjasm string)
  (export sjasm-string)
  (import (micascheme) (code) (sjasm code))

  (define-syntax (sjasm-string $syntax)
    (syntax-case $syntax ()
      ((_ instr ...)
        (code-string (lines-code #'(instr ...))))))
)
