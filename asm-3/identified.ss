(library (asm-3 identified)
  (export
    identified identified? identified-identifier identified-ref
    identified->syntax)
  (import (micascheme))

  (data (identified identifier ref))

  (define (identified->syntax $identified $ref->syntax)
    #`(
      #,(identified-identifier $identified)
      #,($ref->syntax (identified-ref $identified))))
)
