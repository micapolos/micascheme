(library (micac code)
  (export identifier->code)
  (import (micascheme) (code))

  (define (identifier->code $identifier)
    (fluent $identifier
      (syntax->datum)
      (symbol->string)
      (string->list)
      (map-using identifier-char)
      (list->string)
      (string-code)))

  (define (identifier-char $char)
    (case $char
      ((#\- #\? #\%) #\_)
      (else $char)))
)
