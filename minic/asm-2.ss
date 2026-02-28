(library (minic asm-2)
  (export
    asm
    local
    nop block switch loop
    const ld inc dec add sub
    in out)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (minic prim)
    (stack)
    (lets)
    (list-syntax)
    (procedure)
    (only (switch) index-switch))

  (define-keywords
    local
    nop block switch loop
    const ld inc dec add sub
    in out)

  (define-syntax (asm $syntax)
    (syntax-case $syntax ()
      (($id ($io) $op* ...)
        (let ()
          (define (op-syntax-list $id-stack $op)
            (map (partial op-syntax $id-stack) (syntax->list $op)))

          (define (op-syntax $id-stack $op)
            (define (reg-id $offset)
              (list-ref $id-stack $offset))

            (define reg
              (case-lambda
                (($offset)
                  (reg-id $offset))
                (($offset $expr)
                  #`(set! #,(reg-id $offset) #,$expr))))

            (syntax-case $op
              ( ; keywords
                local
                const ld inc dec add sub
                in out
                block switch loop)

              ((local $size $op ...)
                (lets
                  ($tmps (generate-temporaries (iota (datum $size))))
                  #`(let ()
                    #,@(map-with
                      ($tmp $tmps)
                      #`(define #,$tmp 0))
                    #,@(op-syntax-list
                      (push-all $id-stack $tmps)
                      #'($op ...)))))

              ((nop)
                #`(void))
              ((block $op ...)
                #`(begin #,@(op-syntax-list $id-stack #'($op ...))))
              ((switch $lhs $op ...)
                #`(prim-switch #,(reg (datum $lhs))
                  #,@(op-syntax-list $id-stack #'($op ...))))
              ((loop $cond $op ...)
                #`(let loop ()
                  (cond
                    ((prim-zero? #,(reg (datum $cond))) (void))
                    (else #,@(op-syntax-list $id-stack #'($op ...)) (loop)))))

              ((const $offset $fx)
                (reg (datum $offset) #'$fx))
              ((ld $lhs $rhs)
                (reg (datum $lhs) (reg (datum $rhs))))
              ((inc $lhs)
                (reg (datum $lhs) #`(prim+1 #,(reg (datum $lhs)))))
              ((dec $lhs)
                (reg (datum $lhs) #`(prim-1 #,(reg (datum $lhs)))))
              ((add $lhs $rhs)
                (reg (datum $lhs) #`(prim+ #,(reg (datum $lhs)) #,(reg (datum $rhs)))))
              ((sub $lhs $rhs)
                (reg (datum $lhs) #`(prim- #,(reg (datum $lhs)) #,(reg (datum $rhs)))))

              ((in $lhs $rhs)
                (reg (datum $lhs) #`($io #,(reg (datum $rhs)))))
              ((out $lhs $rhs)
                #`($io #,(reg (datum $rhs)) #,(reg (datum $lhs))))))

          #`(begin
            #,@(op-syntax-list (stack) #'($op* ...)))))))
)
