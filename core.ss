(library (core)
  (export cons!)
  (import (scheme))

  (define-syntax cons!
    (syntax-rules ()
      ((_ expr var)
        (identifier? #'var)
        (set! var (cons expr var)))))
)
