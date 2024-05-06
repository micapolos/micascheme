(library (emu-2 emu)
  (export emu)
  (import (scheme) (syntax) (syntaxes) (system))

  (define-rules-syntax
    ((emu unit body ...)
      (implicit step)
      (begin
        (define (step cycles) (displayln `(step ,cycles)))
        (pretty-print `(creating unit))
        (pretty-print `(starting))
        body ...
        (pretty-print `(ending))
        (pretty-print `(done)))))
)
