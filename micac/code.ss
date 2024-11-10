(library (micac code)
  (export identifier->code)
  (import (micascheme) (code))

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
)
