(library (asm-2 relocable)
  (export
    relocable relocable? relocable-org->value-proc
    relocable-with
    relocable-ref
    relocable-map)
  (import (micascheme))

  (data (relocable org->value-proc))

  (define-rules-syntax
    ((relocable-with body)
      (relocable-with (_) body))
    ((relocable-with ($org) body)
      (relocable (lambda ($org) body))))

  (define (relocable-ref $relocable $org)
    ((relocable-org->value-proc $relocable) $org))

  (define (relocable-map $proc $relocable)
    (relocable
      (lambda ($org)
        ($proc (relocable-ref $relocable $org)))))
)
