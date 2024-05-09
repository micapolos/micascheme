(library (minic vm)
  (export
    vm
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

  (define-syntax (vm $syntax)
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
                #`(set! $sp (fast- $sp $size)))
              ((free $size)
                #`(set! $sp (fast+ $sp $size)))

              ((const $offset $fx)
                #`(imem $offset $fx))
              ((ld $lhs $rhs)
                #`(imem $lhs (imem $rhs)))
              ((inc $lhs)
                #`(imem $lhs (fast+ (imem $lhs) 1)))
              ((dec $lhs)
                #`(imem $lhs (fast- (imem $lhs) 1)))
              ((add $lhs $rhs)
                #`(imem $lhs (fast+ (imem $lhs) (imem $rhs))))
              ((sub $lhs $rhs)
                #`(imem $lhs (fast- (imem $lhs) (imem $rhs))))

              ((in $lhs $rhs)
                #`(imem $lhs ($io (imem $rhs))))
              ((out $lhs $rhs)
                #`($io (imem $rhs) (imem $lhs)))

              ((block $op ...)
                #`(begin #,@(map op-syntax (syntax->list #'($op ...)))))
              ((switch $lhs $op ...)
                #`(index-switch (imem $lhs)
                  #,@(map op-syntax (syntax->list #'($op ...)))))
              ((loop $cond $op ...)
                #`(let loop ()
                  (cond
                    ((($primitive 3 fxzero?) (imem $cond)) (void))
                    (else
                      #,@(map op-syntax (syntax->list #'($op ...)))
                      (loop)))))))
          #`(let ()
            (define $fxvector (make-fxvector $stack-size))
            (define $sp (fxvector-length $fxvector))
            (define-rules-syntaxes
              ((fast+ $a $b)
                (($primitive 3 fx+/wraparound) $a $b))
              ((fast- $a $b)
                (($primitive 3 fx-/wraparound) $a $b))
              ((fast-zero? $a)
                (($primitive 3 fxzero?) $a))
              ((iaddr $offset)
                (fast+ $sp $offset))
              ((imem $offset)
                (($primitive 3 fxvector-ref) $fxvector (iaddr $offset)))
              ((imem $offset $fx)
                (($primitive 3 fxvector-set!) $fxvector (iaddr $offset) $fx)))
            #,@(map op-syntax (syntax->list #'($op* ...)))
            (void))))))
)
