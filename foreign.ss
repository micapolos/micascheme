(library (foreign)
  (export
    foreign-alloc-0
    foreign-free-0

    with-foreign-alloc
    with-foreign-alloc-0)
  (import (scheme) (syntaxes) (dynamic-wind))

  (define (foreign-alloc-0 size)
    (if (zero? size) 0 (foreign-alloc size)))

  (define (foreign-free-0 ptr)
    (if (zero? ptr) (void) (foreign-free ptr)))

  (define-rules-syntax
    ((with-foreign-alloc body) body)
    ((with-foreign-alloc (id size) next ... body)
      (with-dynamic-wind
        (id (foreign-alloc size))
        (with-foreign-alloc next ... body)
        (foreign-free id))))

  (define-rules-syntax
    ((with-foreign-alloc-0 body) body)
    ((with-foreign-alloc-0 (id size) next ... body)
      (with-dynamic-wind
        (id (foreign-alloc-0 size))
        (with-foreign-alloc-0 next ... body)
        (foreign-free-0 id))))
)
