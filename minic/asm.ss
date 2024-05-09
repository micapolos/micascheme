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
                #`(set! $sp (fast- $sp $size)))
              ((free $size)
                #`(set! $sp (fast+ $sp $size)))

              ((const $offset $fx)
                #`(reg $offset $fx))
              ((ld $lhs $rhs)
                #`(reg $lhs (reg $rhs)))
              ((inc $lhs)
                #`(reg $lhs (fast+ (reg $lhs) 1)))
              ((dec $lhs)
                #`(reg $lhs (fast- (reg $lhs) 1)))
              ((add $lhs $rhs)
                #`(reg $lhs (fast+ (reg $lhs) (reg $rhs))))
              ((sub $lhs $rhs)
                #`(reg $lhs (fast- (reg $lhs) (reg $rhs))))

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
                    ((fast-zero? (reg $cond)) (void))
                    (else
                      #,@(map op-syntax (syntax->list #'($op ...)))
                      (loop)))))))
          #`(let ()
            (define-rules-syntaxes
              ((fast+ $a $b)
                (($primitive 3 fx+/wraparound) $a $b))
              ((fast- $a $b)
                (($primitive 3 fx-/wraparound) $a $b))
              ((fast-zero? $a)
                (($primitive 3 fxzero?) $a))
              ((iaddr $offset)
                (fast+ $sp $offset))
              ((reg $offset)
                (($primitive 3 fxvector-ref) $reg-fxvector (iaddr $offset)))
              ((reg $offset $fx)
                (($primitive 3 fxvector-set!) $reg-fxvector (iaddr $offset) $fx)))
            #,@(map op-syntax (syntax->list #'($op* ...)))
            (void))))))
)
