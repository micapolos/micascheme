(library (emu-2 emu)
  (export emu)
  (import (scheme) (syntax) (system))

  (define-syntax (emu $syntax)
    (syntax-case $syntax ()
      ((id unit body ...)
        (with-implicit (id step)
          #`(begin
            (define (step cycles) (displayln `(step ,cycles)))
            (pretty-print `(creating unit))
            (pretty-print `(starting))
            body ...
            (pretty-print `(ending))
            (pretty-print `(done)))))))
)
