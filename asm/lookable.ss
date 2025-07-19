(library (asm lookable)
  (export
    lookable lookable-ref
    lookable-map
    list->lookable
    lookable-append
    resolve-lookable)
  (import (micascheme))

  (define-rules-syntax
    ((lookable body)
      (lookable (_) body))
    ((lookable ($lookup) body)
      (lambda ($lookup) body)))

  (define (lookable-ref $lookable $lookup)
    ($lookable $lookup))

  (define (resolve-lookable $lookup $lookable)
    ($lookable $lookup))

  (define (lookable-map $proc $lookable)
    (lookable ($lookup)
      ($proc (lookable-ref $lookable $lookup))))

  (define-list->/append (lookable $lookables)
    (lookable ($lookup)
      (map-with ($lookable $lookables)
        (lookable-ref $lookable $lookup))))
)
