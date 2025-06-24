(library (typico literal-expand)
  (export literal-expand-typed)
  (import (micascheme) (typico typed) (typico core-types))

  (define (literal-expand-typed $syntax)
    (syntax-case $syntax ()
      (b
        (boolean? (datum b))
        (typed boolean-type (datum b)))
      (i
        (integer? (datum i))
        (typed integer-type (datum i)))
      (ch
        (char? (datum ch))
        (typed char-type (datum ch)))
      (s
        (string? (datum s))
        (typed string-type (datum s)))))
)
