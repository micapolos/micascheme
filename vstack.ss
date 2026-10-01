(library (vstack)
  (export
    with-vstack
    with-vstack-alloc
    vstack-alloc
    vstack-u8-ref
    vstack-u8-set!
    vstack-u32-ref
    vstack-u32-set!)

  (import
    (scheme)
    (syntax)
    (foreign))

  (define-rule-syntax (with-vstack (vstack size) x xs ...)
    (with-foreign-alloc (vstack size) x xs ...))

  (define-rule-syntax (vstack-alloc vstack size)
    (fx-/wraparound vstack size))

  (define-rule-syntax (with-vstack-alloc (vstack size) x xs ...)
    (let
      ((vstack (vstack-alloc vstack size)))
      x xs ...))

  (define-rule-syntax (vstack-u8-ref vstack offset)
    (foreign-ref 'unsigned-8 vstack offset))

  (define-rule-syntax (vstack-u8-set! vstack offset u8)
    (foreign-set! 'unsigned-8 vstack offset u8))

  (define-rule-syntax (vstack-u32-ref vstack offset)
    (foreign-ref 'unsigned-32 vstack offset))

  (define-rule-syntax (vstack-u32-set! vstack offset u32)
    (foreign-set! 'unsigned-32 vstack offset u32))
)
