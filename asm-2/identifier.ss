(library (asm-2 identifier)
  (export syntax->identifier? syntax->identifier)
  (import (micascheme))

  (define (syntax->identifier? $syntax)
    (syntax-case? $syntax ()
      (id (identifier? #'id) #'id)
      ((id . rest) (identifier? #'id) #'id)))

  (define (syntax->identifier $syntax)
    (or
      (syntax->identifier? $syntax)
      (syntax-error $syntax "clause without identifier")))
)
