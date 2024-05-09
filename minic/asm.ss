(library (minic asm)
  (export
    asm
    alloc free
    const ld inc dec add sub
    in out
    block switch loop)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (minic prim)
    (only (switch) index-switch))

  (define-aux-keywords
    alloc free
    const ld inc dec add sub
    in out
    block switch loop)

  (define-syntax (asm $syntax)
    (syntax-case $syntax ()
      (($id ($reg-fxvector $sp $io) $op* ...)
        (let ()
          (define (op-syntax $op)
            (syntax-case $op
              ( ; literals
                alloc free
                const ld inc dec add sub
                in out
                block switch loop)

              ((alloc $size)
                #`(set! $sp (prim- $sp $size)))
              ((free $size)
                #`(set! $sp (prim+ $sp $size)))

              ((const $offset $fx)
                #`(reg $offset $fx))
              ((ld $lhs $rhs)
                #`(reg $lhs (reg $rhs)))
              ((inc $lhs)
                #`(reg $lhs (prim+1 (reg $lhs))))
              ((dec $lhs)
                #`(reg $lhs (prim-1 (reg $lhs))))
              ((add $lhs $rhs)
                #`(reg $lhs (prim+ (reg $lhs) (reg $rhs))))
              ((sub $lhs $rhs)
                #`(reg $lhs (prim- (reg $lhs) (reg $rhs))))

              ((in $lhs $rhs)
                #`(reg $lhs ($io (reg $rhs))))
              ((out $lhs $rhs)
                #`($io (reg $rhs) (reg $lhs)))

              ((block $op ...)
                #`(begin #,@(map op-syntax (syntax->list #'($op ...)))))
              ((switch $lhs $op ...)
                #`(index-switch (reg $lhs)
                  #,@(map op-syntax (syntax->list #'($op ...)))))
              ((loop $cond $op ...)
                #`(let loop ()
                  (cond
                    ((prim-zero? (reg $cond)) (void))
                    (else
                      #,@(map op-syntax (syntax->list #'($op ...)))
                      (loop)))))))
          #`(let ()
            (define-rules-syntaxes
              ((reg-addr $offset)
                (prim+ $sp $offset))
              ((reg $offset)
                (prim-ref $reg-fxvector (reg-addr $offset)))
              ((reg $offset $fx)
                (prim-set! $reg-fxvector (reg-addr $offset) $fx)))
            #,@(map op-syntax (syntax->list #'($op* ...)))
            (void))))))
)
