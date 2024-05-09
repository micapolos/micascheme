(library (minic stako)
  (export
    stako
    alloc free
    const-8 load-8 inc-8 dec-8 add-8 sub-8 switch-8 loop-8
    const-16 load-16 inc-16 dec-16 add-16 sub-16 switch-16 loop-16
    block)
  (import
    (except (micascheme) push pop)
    (minic runtime))

  (define-aux-keywords
    alloc free
    const-8 load-8 inc-8 dec-8 add-8 sub-8 switch-8 loop-8
    const-16 load-16 inc-16 dec-16 add-16 sub-16 switch-16 loop-16
    block)

  (define-syntax (stako $syntax)
    (syntax-case $syntax ()
      (($id $extra-size-expr $in-expr $op* ...)
        (let ()
          (define (op-syntax $op)
            (syntax-case $op
              (; keywords
                alloc free
                const-8 load-8 inc-8 dec-8 add-8 sub-8 switch-8 loop-8
                const-16 load-16 inc-16 dec-16 add-16 sub-16 switch-16 loop-16
                block)
              ((alloc $size)
                #`(set! $sp (- $sp $size)))
              ((free $size)
                #`(set! $sp (+ $sp $size)))

              ((const-8 $offset $u8)
                #`(imem-8 $offset $u8))
              ((load-8 $lhs $rhs)
                #`(imem-8 $lhs (imem-8 $rhs)))
              ((inc-8 $rhs)
                #`(imem-8 $rhs (int 8 inc (imem-8 $rhs))))
              ((dec-8 $rhs)
                #`(imem-8 $rhs (int 8 dec (imem-8 $rhs))))
              ((add-8 $lhs $rhs)
                #`(imem-8 $lhs (int 8 add (imem-8 $lhs) (imem-8 $rhs))))
              ((sub-8 $lhs $rhs)
                #`(imem-8 $lhs (int 8 sub (imem-8 $lhs) (imem-8 $rhs))))
              ((switch-8 $lhs $op ...)
                #`(index-switch (imem-8 $lhs)
                  #,@(map op-syntax (syntax->list #'($op ...)))))
              ((loop-8 $cond $op ...)
                #`(let loop ()
                  (cond
                    ((zero? (imem-8 $cond)) (void))
                    (else
                      #,@(map op-syntax (syntax->list #'($op ...)))
                      (loop)))))

              ((const-16 $offset $u16)
                #`(imem-16 $offset $u16))
              ((load-16 $lhs $rhs)
                #`(imem-16 $lhs (imem-16 $rhs)))
              ((inc-16 $rhs)
                #`(imem-16 $rhs (int 16 inc (imem-16 $rhs))))
              ((dec-16 $rhs)
                #`(imem-16 $rhs (int 16 dec (imem-16 $rhs))))
              ((add-16 $lhs $rhs)
                #`(imem-16 $lhs (int 16 add (imem-16 $lhs) (imem-16 $rhs))))
              ((sub-16 $lhs $rhs)
                #`(imem-16 $lhs (int 16 sub (imem-16 $lhs) (imem-16 $rhs))))
              ((switch-16 $lhs $op ...)
                #`(index-switch (imem-16 $lhs)
                  #,@(map op-syntax (syntax->list #'($op ...)))))
              ((loop-16 $cond $op ...)
                #`(let loop ()
                  (cond
                    ((zero? (imem-16 $cond)) (void))
                    (else
                      #,@(map op-syntax (syntax->list #'($op ...)))
                      (loop)))))

              ((block $op ...)
                #`(begin #,@(map op-syntax (syntax->list #'($op ...)))))))
          #`(let ()
            (define $in $in-expr)
            (define $in-size (bytevector-length $in))
            (define $extra-size $extra-size-expr)
            (define $size (+ $extra-size $in-size))
            (define $sp $extra-size)
            (define $bytevector (make-bytevector $size))
            (define-rules-syntaxes
              ((iaddr $offset)
                (+ $sp $offset))
              ((imem-8 $offset)
                (bytevector-u8-ref $bytevector (iaddr $offset)))
              ((imem-8 $offset $u8)
                (bytevector-u8-set! $bytevector (iaddr $offset) $u8))
              ((imem-16 $offset)
                (bytevector-u16-ref $bytevector (iaddr $offset)))
              ((imem-16 $offset $u16)
                (bytevector-u16-set! $bytevector (iaddr $offset) $u16)))
            (bytevector-copy! $in 0 $bytevector $sp $in-size)
            #,@(map op-syntax (syntax->list #'($op* ...)))
            (lets
              ($out-size (- $size $sp))
              ($out (make-bytevector $out-size))
              (run (bytevector-copy! $bytevector $sp $out 0 $out-size))
              $out))))))
)
