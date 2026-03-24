(library (leo word)
  (export symbol->word)
  (import (micascheme))

  (define (symbol->word $symbol)
    (case $symbol
      ((quasiquote) "<<")
      ((unquote) ">>")
      ((unquote-splicing) ">>...")
      (else (symbol->string $symbol))))
)
