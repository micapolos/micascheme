(library (asm identified)
  (export
    identified identified? identified-identifier identified-ref
    identified-with
    identified->syntax
    identified-map
    map-identified
    identified->datum
    identified->entry-datum
    identified-identifier=?)
  (import (asm base))

  (define-annotated (identified identifier))

  (define-rule-syntax (identified-with id ref)
    (identified #'id ref))

  (define (identified->syntax $identified $ref->syntax)
    #`(
      #,(identified-identifier $identified)
      #,($ref->syntax (identified-ref $identified))))

  (define (identified->datum $ref->datum $identified)
    `(identified
      ,(syntax->datum (identified-identifier $identified))
      ,($ref->datum (identified-ref $identified))))

  (define (identified->entry-datum $identified)
    `(,(syntax->datum (identified-identifier $identified))
      ,(identified-ref $identified)))

  (define (identified-identifier=? $identified $identifier)
    (free-identifier=? $identifier (identified-identifier $identified)))
)
