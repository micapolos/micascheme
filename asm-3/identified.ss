(library (asm-3 identified)
  (export
    identified identified? identified-identifier identified-ref
    identified-with
    identified->syntax
    identified-map
    identified->datum
    identified-identifier=?)
  (import (micascheme))

  (data (identified identifier ref))

  (define-rule-syntax (identified-with id ref)
    (identified #'id ref))

  (define (identified->syntax $identified $ref->syntax)
    #`(
      #,(identified-identifier $identified)
      #,($ref->syntax (identified-ref $identified))))

  (define (identified-map $proc $identified)
    (identified-with-ref $identified
      ($proc (identified-ref $identified))))

  (define (identified->datum $ref->datum $identified)
    `(identified
      ,(syntax->datum (identified-identifier $identified))
      ,($ref->datum (identified-ref $identified))))

  (define (identified-identifier=? $identified $identifier)
    (free-identifier=? $identifier (identified-identifier $identified)))
)
