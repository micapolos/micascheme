(library (asm lookable)
  (export
    lookable lookable-ref
    pure-lookable
    lookable-map
    list->lookable
    lookable-append
    lookable-append-map
    resolve-lookable)
  (import (micascheme))

  (define-rules-syntax
    ((lookable body)
      (lookable (_) body))
    ((lookable ($lookup) body)
      (lambda ($lookup) body)))

  (define (pure-lookable $obj)
    (lookable $obj))

  (define (lookable-ref $lookable $lookup)
    ($lookable $lookup))

  (define (resolve-lookable $lookup $lookable)
    ($lookable $lookup))

  (define (lookable-map $lookable $proc)
    (lookable ($lookup)
      ($proc (lookable-ref $lookable $lookup))))

  (define-list->/append (lookable $lookables)
    (lookable ($lookup)
      (map-with ($lookable $lookables)
        (lookable-ref $lookable $lookup))))

  (define (lookable-append-map $ref-append . $lookables)
    (lookable-map
      (apply lookable-append $lookables)
      (partial apply $ref-append)))
)
