(library (foreign)
  (export
    foreign-alloc-0
    foreign-free-0

    with-foreign-alloc
    with-foreign-alloc-0)
  (import (scheme) (syntax) (dynamic-wind))

  (define (foreign-alloc-0 size)
    (if (zero? size) 0 (foreign-alloc size)))

  (define (foreign-free-0 ptr)
    (if (zero? ptr) (void) (foreign-free ptr)))

  (define-rule-syntax (with-foreign-alloc (id size) body ...)
    (with-dynamic-wind
      (id (foreign-alloc size))
      (begin body ...)
      (foreign-free id)))

  (define-rule-syntax (with-foreign-alloc-0 (id size) body ...)
    (with-dynamic-wind
      (id (foreign-alloc-0 size))
      (begin body ...)
      (foreign-free-0 id)))
)
