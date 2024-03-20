(library (asm asm)
  (export
    asm-bytevector
    label align org db dw ds)
  (import (micascheme))

  (define-aux-keyword label)
  (define-aux-keyword align)
  (define-aux-keyword org)
  (define-aux-keyword db)
  (define-aux-keyword dw)
  (define-aux-keyword ds)

  (define-syntax asm-bytevector
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          (($asm $op ...)
            (with-implicit ($asm emit)
              (let ()
                (define $let-entries (stack))
                (define $statements (stack))
                (define $pc 0)
                (define (push-let-entry! $let-entry)
                  (set! $let-entries (push $let-entries $let-entry)))
                (define (push-statement! $statement)
                  (set! $statements (push $statements $statement)))
                (define (size? $datum)
                  (and (integer? $datum) (nonnegative? $datum)))
                (for-each
                  (lambda ($op)
                    (syntax-case $op (label align org db dw ds)
                      ((db $expr)
                        (run
                          (push-statement! #`(emit $expr))
                          (set! $pc (+ $pc 1))))
                      ((dw $expr)
                        (run
                          (push-statement!
                            #`(let (($value $expr))
                              (emit (fxand $value #xff))
                              (emit (fxsrl $value 8))))
                          (set! $pc (+ $pc 2))))
                      ((ds $expr) (size? (datum $expr))
                        (run
                          (push-statement! #`(repeat $expr (emit 0)))
                          (set! $pc (+ $pc (datum $expr)))))
                      ((label $name) (identifier? #'$name)
                        (push-let-entry! #`($name #,$pc)))
                      ((align $expr) (size? (datum $expr))
                        (lets
                          ($new-pc (bitwise-align $pc (datum $expr)))
                          ($slack (- $new-pc $pc))
                          (run
                            (if (not (zero? $slack))
                              (run
                                (push-statement! #`(repeat #,$slack (emit 0)))
                                (set! $pc $new-pc))))))
                      ((org $expr) (size? (datum $expr))
                        (set! $pc (datum $expr)))))
                  (syntax->list #'($op ...)))
                #`(lets
                  ((values $port $close) (open-bytevector-output-port))
                  (emit (lambda ($u8) (put-u8 $port $u8)))
                  (let (#,@(reverse $let-entries))
                    #,@(reverse $statements)
                    ($close))))))))))
)
