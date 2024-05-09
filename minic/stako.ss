(library (minic stako)
  (export
    stako
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

  (define-syntax (stako $syntax)
    (syntax-case $syntax ()
      (($id ($stack-size $io) $op* ...)
        (let ()
          (define (op-syntax $op)
            (syntax-case $op
              ( ; literals
                alloc free
                const ld inc dec add sub
                in out
                block switch loop)
              ((alloc $size)
                #`(set! $sp (- $sp $size)))
              ((free $size)
                #`(set! $sp (+ $sp $size)))

              ((const $offset $fx)
                #`(imem $offset $fx))
              ((ld $lhs $rhs)
                #`(imem $lhs (imem $rhs)))
              ((inc $lhs)
                #`(imem $lhs (fx+/wraparound (imem $lhs) 1)))
              ((dec $lhs)
                #`(imem $lhs (fx-/wraparound (imem $lhs) 1)))
              ((add $lhs $rhs)
                #`(imem $lhs (fx+/wraparound (imem $lhs) (imem $rhs))))
              ((sub $lhs $rhs)
                #`(imem $lhs (fx-/wraparound (imem $lhs) (imem $rhs))))

              ((in $lhs $rhs)
                #`(imem $rhs ($io (imem $lhs))))
              ((out $lhs $rhs)
                #`($io (imem $lhs) (imem $rhs)))

              ((block $op ...)
                #`(begin #,@(map op-syntax (syntax->list #'($op ...)))))
              ((switch $lhs $op ...)
                #`(index-switch (imem $lhs)
                  #,@(map op-syntax (syntax->list #'($op ...)))))
              ((loop $cond $op ...)
                #`(let loop ()
                  (cond
                    ((fxzero? (imem $cond)) (void))
                    (else
                      #,@(map op-syntax (syntax->list #'($op ...)))
                      (loop)))))))
          #`(let ()
            (define $fxvector (make-fxvector $stack-size))
            (define $sp (fxvector-length $fxvector))
            (define-rules-syntaxes
              ((iaddr $offset)
                (+ $sp $offset))
              ((imem $offset)
                (fxvector-ref $fxvector (iaddr $offset)))
              ((imem $offset $fx)
                (fxvector-set! $fxvector (iaddr $offset) $fx)))
            #,@(map op-syntax (syntax->list #'($op* ...)))
            (void))))))
)
