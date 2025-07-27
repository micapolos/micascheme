(library (asm-3 relocable)
  (export
    relocable relocable? relocable-proc
    pure-relocable
    org-relocable
    relocable-with
    relocable-ref
    locate-relocable
    relocable-map
    map-relocable
    relocable+offset
    offset-relocable
    list->relocable
    relocable-append
    relocable-append-map
    check-relocable)
  (import (micascheme))

  (data (relocable proc))

  (define-rules-syntax
    ((relocable-with body)
      (relocable-with (_) body))
    ((relocable-with ($org) body)
      (relocable (lambda ($org) body))))

  (define (pure-relocable $ref)
    (relocable-with $ref))

  (define (org-relocable)
    (relocable-with ($org) $org))

  (define (relocable-ref $relocable $org)
    ((relocable-proc $relocable) $org))

  (define (locate-relocable $org $relocable)
    ((relocable-proc $relocable) $org))

  (define (relocable-map $relocable $proc)
    (relocable
      (lambda ($org)
        ($proc (relocable-ref $relocable $org)))))

  (define (map-relocable $proc $relocable)
    (relocable-map $relocable $proc))

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

  (define-rule-syntax (check-relocable org relocable out)
    (check (equal? (relocable-ref relocable org) out)))
)
