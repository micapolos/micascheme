(library (leo quotes)
  (export
    char->quote?
    char->unquote?)
  (import (scheme))

  (define (char->quote? $char)
    (case $char
      ((#\') 'quote)
      ((#\`) 'quasiquote)
      (else #f)))

  (define (char->unquote? $char)
    (case $char
      ((#\`) 'unquote)
      (else #f)))

)
