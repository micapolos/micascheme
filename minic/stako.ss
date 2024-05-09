(library (minic stako)
  (export
    stako
    const-8 inc-8 dec-8 add-8 sub-8 out-8
    block switch-8)
  (import
    (except (micascheme) push pop)
    (minic runtime))

  (define-aux-keywords
    const-8 inc-8 dec-8 add-8 sub-8 out-8
    block switch-8)

  (define-syntax (stako $syntax)
    (syntax-case $syntax ()
      (($id $size $op* ...)
        (let ()
          (define (op-syntax $op)
            (syntax-case $op (const-8 inc-8 dec-8 add-8 sub-8 out-8 block switch-8)
              ((const-8 $u8) #`(push (int 8 const $u8)))
              ((inc-8) #`(top (int 8 inc (top))))
              ((dec-8) #`(top (int 8 dec (top))))
              ((add-8) #`(let (($rhs (pop))) (top (int 8 add (top) $rhs))))
              ((sub-8) #`(let (($rhs (pop))) (top (int 8 sub (top) $rhs))))
              ((out-8) #`(displayln (pop)))
              ((block $op ...) #`(begin #,@(map op-syntax (syntax->list #'($op ...)))))
              ((switch-8 $op ...) #`(index-switch (pop) #,@(map op-syntax (syntax->list #'($op ...)))))))
          #`(let ()
            (define $sp $size)
            (define $bytevector (make-bytevector $size))
            (define-rules-syntaxes
              ((mem $addr) (bytevector-u8-ref $bytevector $addr))
              ((mem $addr $u8) (bytevector-u8-set! $bytevector $addr $u8))
              ((iaddr $offset) (+ $sp $offset))
              ((imem $offset) (bytevector-u8-ref $bytevector (iaddr $offset)))
              ((imem $offset $u8) (bytevector-u8-set! $bytevector (iaddr $offset)))
              ((top) (mem $sp))
              ((top $u8) (mem $sp $u8))
              ((push $u8) (let () (set! $sp (- $sp 1)) (top $u8)))
              ((pop) (let (($u8 (mem $sp))) (set! $sp (+ $sp 1)) $u8)))
            #,@(map op-syntax (syntax->list #'($op* ...)))
            (pop))))))
)
