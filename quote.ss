(library (quote)
  (export quote-operator)
  (import (scheme))

  (define-syntax quote-operator
    (syntax-rules ()
      ((_ (op arg ...))
        `(op ,arg ...))))
)
