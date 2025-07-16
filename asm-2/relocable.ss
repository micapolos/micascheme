(library (asm-2 relocable)
  (export
    relocable relocable? relocable-org->value-proc
    relocable-with
    relocable->value
    relocable-map)
  (import (micascheme))

  (data (relocable org->value-proc))

  (define-rules-syntax
    ((relocable-with body)
      (relocable-with (_) body))
    ((relocable-with ($org) body)
      (relocable (lambda ($org) body))))

  (define (relocable->value $relocable $org)
    ((relocable-org->value-proc $relocable) $org))

  (define (relocable-map $proc $relocable)
    (relocable
      (lambda ($org)
        ($proc (relocable->value $relocable $org)))))
)
