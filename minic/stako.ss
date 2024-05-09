(library (minic stako)
  (export
    stako
    alloc free
    const-8 inc-8 dec-8 add-8 sub-8
    block switch-8)
  (import
    (except (micascheme) push pop)
    (minic runtime))

  (define-aux-keywords
    alloc free
    const-8 inc-8 dec-8 add-8 sub-8
    block switch-8)

  (define-syntax (stako $syntax)
    (syntax-case $syntax ()
      (($id $extra-size-expr $in-expr $op* ...)
        (let ()
          (define (op-syntax $op)
            (syntax-case $op (alloc free const-8 inc-8 dec-8 add-8 sub-8 block switch-8)
              ((alloc $size)
                #`(set! $sp (- $sp $size)))
              ((free $size)
                #`(set! $sp (+ $sp $size)))
              ((const-8 $offset $u8)
                #`(imem $offset $u8))
              ((inc-8 $rhs)
                #`(imem $rhs (int 8 inc (imem $rhs))))
              ((dec-8 $rhs)
                #`(imem $rhs (int 8 dec (imem $rhs))))
              ((add-8 $lhs $rhs)
                #`(imem $lhs (int 8 add (imem $lhs) (imem $rhs))))
              ((sub-8 $lhs $rhs)
                #`(imem $lhs (int 8 sub (imem $lhs) (imem $rhs))))
              ((block $op ...)
                #`(begin #,@(map op-syntax (syntax->list #'($op ...)))))
              ((switch-8 $lhs $op ...)
                #`(index-switch (imem $lhs)
                  #,@(map op-syntax (syntax->list #'($op ...)))))))
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
              ((imem $offset)
                (bytevector-u8-ref $bytevector (iaddr $offset)))
              ((imem $offset $u8)
                (bytevector-u8-set! $bytevector (iaddr $offset) $u8)))
            (bytevector-copy! $in 0 $bytevector $sp $in-size)
            #,@(map op-syntax (syntax->list #'($op* ...)))
            (lets
              ($out-size (- $size $sp))
              ($out (make-bytevector $out-size))
              (run (bytevector-copy! $bytevector $sp $out 0 $out-size))
              $out))))))
)
