(library (minic asm)
  (export
    asm
    local block switch loop
    const ld inc dec add sub
    in out)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (minic prim)
    (only (switch) index-switch))

  (define-aux-keywords
    local block switch loop
    const ld inc dec add sub
    in out)

  (define-syntax (asm $syntax)
    (syntax-case $syntax ()
      (($id ($size $io) $op* ...)
        (let ()
          (define (op-syntax-list $op)
            (map op-syntax (syntax->list $op)))

          (define (op-syntax $op)
            (syntax-case $op
              ( ; literals
                local
                const ld inc dec add sub
                in out
                block switch loop)

              ((local $size $op ...)
                #`(begin
                  (set! $sp (prim- $sp $size))
                  #,@(op-syntax-list #'($op ...))
                  (set! $sp (prim+ $sp $size))))
              ((block $op ...)
                #`(begin #,@(op-syntax-list #'($op ...))))
              ((switch $lhs $op ...)
                #`(index-switch (reg $lhs)
                  #,@(op-syntax-list #'($op ...))))
              ((loop $cond $op ...)
                #`(let loop ()
                  (cond
                    ((prim-zero? (reg $cond)) (void))
                    (else #,@(op-syntax-list #'($op ...)) (loop)))))

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
                #`($io (reg $rhs) (reg $lhs)))))
          #`(let ()
            (define $regs (make-fxvector $size))
            (define $sp (fxvector-length $regs))
            (define-rules-syntaxes
              ((reg-addr $offset)
                (prim+ $sp $offset))
              ((reg $offset)
                (prim-ref $regs (reg-addr $offset)))
              ((reg $offset $fx)
                (prim-set! $regs (reg-addr $offset) $fx)))
            #,@(map op-syntax (syntax->list #'($op* ...)))
            (void))))))
)
