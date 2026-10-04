(library (vstack)
  (export
    with-vstack
    with-vstack-alloc
    vstack-alloc
    vstack-let
    vstack-u8-ref
    vstack-u8-set!
    vstack-u32-ref
    vstack-u32-set!

    with/sp
    define/sp
    alloc/sp
    ftype/sp)

  (import
    (scheme)
    (lets)
    (syntax)
    (syntaxes)
    (foreign)
    (identifier)
    (fixnum)
    (throw)
    (scoped)
    (keyword))

  (define-rule-syntax (with-vstack (vstack size) x xs ...)
    (lets
      ($size size)
      ($ptr (foreign $size))
      (vstack (fx+/wraparound $ptr $size))
      (begin x xs ...)))

  (define-rule-syntax (vstack-alloc vstack size)
    (fx-/wraparound vstack size))

  (define-rule-syntax (with-vstack-alloc (vstack size) x xs ...)
    (let
      ((vstack (vstack-alloc vstack size)))
      x xs ...))

  (define-rules-syntax
    ((vstack-let vstack body)
      body)
    ((vstack-let vstack (id size) . xs)
      (lets
        (vstack (vstack-alloc vstack size))
        (id vstack)
        (vstack-let vstack . xs))))

  (define-rule-syntax (vstack-u8-ref vstack offset)
    (foreign-ref 'unsigned-8 vstack offset))

  (define-rule-syntax (vstack-u8-set! vstack offset u8)
    (foreign-set! 'unsigned-8 vstack offset u8))

  (define-rule-syntax (vstack-u32-ref vstack offset)
    (foreign-ref 'unsigned-32 vstack offset))

  (define-rule-syntax (vstack-u32-set! vstack offset u32)
    (foreign-set! 'unsigned-32 vstack offset u32))

  (define-syntax (with/sp $syntax)
    (syntax-case $syntax ()
      ((tpl size-expr . body)
        (with-implicit (tpl sp-min sp)
          #`(lets
            (size size-expr)
            (sp-min (foreign size))
            (sp (fx+/wraparound sp-min size))
            (begin . body))))))

  (define-scoped alloc/sp
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ((var (_ size))) body)
          (with-implicit (var sp-min sp)
            #'(let* ((var (fx-/wraparound sp size)) (sp var))
              body))))))

  (define-scoped ftype/sp
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ((var (_ ftype))) body)
          (with-implicit (var sp-min sp)
            #'(let* ((var (fx-/wraparound sp (ftype-sizeof ftype))) (sp var))
              body))))))

  (define-syntax (define/sp $syntax)
    (syntax-case $syntax ()
      ((tpl (id . params) x xs ...)
        (with-implicit (tpl sp-min sp)
          (with-syntax ((id/sp (keyword-append tpl id /sp)))
            #`(begin
              (define (id/sp sp-min sp . params) x xs ...)
              (define-syntax (id $syntax)
                (syntax-case $syntax ()
                  ((tpl . args)
                    (with-implicit (tpl sp-min sp)
                      #'(id/sp sp-min sp . args)))))))))))
)
