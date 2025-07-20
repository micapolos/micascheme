(library (asm-2 relocable)
  (export
    relocable relocable? relocable-proc
    pure-relocable
    relocable-with
    relocable-ref
    locate-relocable
    relocable-map
    relocable+offset
    offset-relocable
    list->relocable
    relocable-append
    relocable-append-map)
  (import (micascheme))

  (data (relocable proc))

  (define-rules-syntax
    ((relocable-with body)
      (relocable-with (_) body))
    ((relocable-with ($org) body)
      (relocable (lambda ($org) body))))

  (define (pure-relocable $ref)
    (relocable-with $ref))

  (define (relocable-ref $relocable $org)
    ((relocable-proc $relocable) $org))

  (define (locate-relocable $org $relocable)
    ((relocable-proc $relocable) $org))

  (define (relocable-map $relocable $proc)
    (relocable
      (lambda ($org)
        ($proc (relocable-ref $relocable $org)))))

  (define (relocable+offset $relocable $offset)
    (relocable-with ($org)
      (relocable-ref $relocable
        (+ $org $offset))))

  (define (offset-relocable $offset $relocable)
    (relocable+offset $relocable $offset))

  (define (list->relocable $relocables)
    (relocable-with ($org)
      (map-with ($relocable $relocables)
        (relocable-ref $relocable $org))))

  (define (relocable-append . $relocables)
    (list->relocable $relocables))

  (define (relocable-append-map $ref-append . $relocables)
    (relocable-map
      (apply relocable-append $relocables)
      (partial apply $ref-append)))
)
