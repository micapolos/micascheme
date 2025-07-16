(library (asm-2 relocable)
  (export
    relocable relocable? relocable-proc
    relocable-with
    relocable-ref
    relocable-map)
  (import (micascheme))

  (data (relocable proc))

  (define-rules-syntax
    ((relocable-with body)
      (relocable-with (_) body))
    ((relocable-with ($org) body)
      (relocable (lambda ($org) body))))

  (define (relocable-ref $relocable $org)
    ((relocable-proc $relocable) $org))

  (define (relocable-map $proc $relocable)
    (relocable
      (lambda ($org)
        ($proc (relocable-ref $relocable $org)))))
)
