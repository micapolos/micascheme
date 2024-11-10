(library (micac identifier)
  (export identifier->expr)
  (import (micascheme) (micac expr))

  (define (identifier->code $identifier)
    (string-code
      (list->string
        (map-with
          ($char
            (string->list
              (symbol->string
                (syntax->datum $identifier))))
          (case $char
            ((#\- #\?) #\_)
            (else $char))))))

  (define (identifier->expr $variable)
    (expr 0 #t (identifier->code $variable)))
)
