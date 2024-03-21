(library (asm asm)
  (export
    define-asm-syntax
    define-asm-syntax-rule
    asm
    asm-bytevector
    label align org db dw ds eq)
  (import
    (micascheme)
    (labs syntax))

  (define-aux-keyword asm-syntax)

  (define-syntax-rule (define-asm-syntax $name $transformer)
    (define-property $name asm-syntax $transformer))

  (define-syntax-rule (define-asm-syntax-rule ($name $param ...) $body)
    (define-asm-syntax $name
      (lambda ($syntax)
        (syntax-case $syntax ()
          ((_ $param ...) #`$body)))))

  (define-aux-keyword label)
  (define-aux-keyword align)
  (define-aux-keyword org)
  (define-aux-keyword db)
  (define-aux-keyword dw)
  (define-aux-keyword ds)
  (define-aux-keyword eq)

  (define-syntax asm
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          (($asm $op ...)
            (with-implicit ($asm $emit $org)
              (let ()
                (define $defines (stack))
                (define $statements (stack))
                (define (push-define! $define)
                  (set! $defines (push $defines $define)))
                (define (push-statement! $statement)
                  (set! $statements (push $statements $statement)))
                (define (size? $datum)
                  (and (integer? $datum) (nonnegative? $datum)))
                (for-each
                  (rec $rec
                    (lambda ($op)
                      (syntax-case $op (eq label align org db dw ds)
                        ((eq $name $expr)
                          (identifier? #'$name)
                          (push-define! #'(define $name $expr)))
                        ((db $expr ...)
                          (for-each
                            (lambda ($expr)
                              (push-statement! #`($emit #,$expr))
                              (push-statement! #`($org (add1 ($org)))))
                            (syntax->list #'($expr ...))))
                        ((dw $expr ...)
                          (for-each
                            (lambda ($expr)
                              (push-statement!
                                #`(let (($value #,$expr))
                                  ($emit (fxand $value #xff))
                                  ($emit (fxsrl $value 8))))
                              (push-statement! #`($org (+ ($org) 2))))
                            (syntax->list #'($expr ...))))
                        ((ds $expr) (size? (datum $expr))
                          (run
                            (push-statement! #`(repeat $expr ($emit 0)))
                            (push-statement! #`($org (+ ($org) $expr)))))
                        ((label $name) (identifier? #'$name)
                          (run
                            (push-define! #`(define $name #f))
                            (push-statement! #`(set! $name ($org)))))
                        ((align $expr) (size? (datum $expr))
                          (push-statement!
                            #`(lets
                              ($new-pc (bitwise-align $pc $expr))
                              ($slack (- $new-pc ($org)))
                              (run
                                (repeat $slack ($emit 0))
                                ($org $new-pc)))))
                        ((org $expr) (size? (datum $expr))
                          (push-statement! #`($org $expr)))
                        (($id $body ...)
                          (and (identifier? #'$id) ($lookup #'$id #'asm-syntax))
                          (for-each $rec
                            (syntax-flatten
                              (($lookup #'$id #'asm-syntax) $op)))))))
                  (syntax->list #'($op ...)))
                #`(lambda ($emit $org)
                  #,@(reverse $defines)
                  #,@(reverse $statements)))))))))

  (define (asm-bytevector $asm)
    (lets
      ((values $port $close) (open-bytevector-output-port))
      ($emit (lambda ($u8) (put-u8 $port $u8)))
      ($org (make-thread-parameter 0))
      (run ($asm $emit $org))
      ($close)))
)
