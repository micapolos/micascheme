(library (asm core)
  (export
    define-asm-core-syntax
    define-asm-core-syntax-rule
    define-asm-syntax
    define-asm-syntax-rule
    asm
    asm-bytevector
    label align eq)
  (import
    (micascheme)
    (labs syntax))

  (define-aux-keyword asm-core-syntax)

  (define-syntax-rule (define-asm-core-syntax $name $transformer)
    (begin
      (define-aux-keyword $name)
      (define-property $name asm-core-syntax $transformer)))

  (define-syntax-rule (define-asm-core-syntax-rule ($name $param ...) $body)
    (define-asm-core-syntax $name
      (lambda ($syntax $emit $org)
        (syntax-case $syntax ()
          ((_ $param ...) #`$body)))))

  (define-aux-keyword asm-syntax)

  (define-syntax-rule (define-asm-syntax $name $transformer)
    (begin
      (define-aux-keyword $name)
      (define-property $name asm-syntax $transformer)))

  (define-syntax-rule (define-asm-syntax-rule ($name $param ...) $body)
    (define-asm-syntax $name
      (lambda ($syntax)
        (syntax-case $syntax ()
          ((_ $param ...) #`$body)))))

  (define-aux-keyword label)
  (define-aux-keyword align)
  (define-aux-keyword ds)
  (define-aux-keyword eq)

  (define-syntax asm
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          (($asm $op ...)
            (with-implicit ($asm $emit)
              (let ()
                (define $defines (stack))
                (define $statements (stack))
                (define (push-define! $define)
                  (set! $defines (push $defines $define)))
                (define (push-statement! $statement)
                  (set! $statements (push $statements $statement)))
                (define (size? $datum)
                  (and (integer? $datum) (nonnegative? $datum)))
                (define $org (make-parameter 0))
                (for-each
                  (rec $rec
                    (lambda ($op)
                      (syntax-case $op (eq label align db dw ds)
                        ((eq $name $expr)
                          (identifier? #'$name)
                          (push-define! #'(define $name $expr)))
                        ((label $name) (identifier? #'$name)
                          (push-define! #`(define $name #,($org))))
                        ((align $expr) (size? (datum $expr))
                          (lets
                            ($pc ($org))
                            ($new-pc (bitwise-align $pc (datum $expr)))
                            ($slack (- $new-pc $pc))
                            (run
                              (push-statement! #`(repeat #,$slack ($emit 0)))
                              ($org $new-pc))))
                        (($id $body ...)
                          (and (identifier? #'$id) ($lookup #'$id #'asm-core-syntax))
                          (for-each push-statement!
                            (syntax-flatten
                              (($lookup #'$id #'asm-core-syntax) $op #'$emit $org))))
                        (($id $body ...)
                          (and (identifier? #'$id) ($lookup #'$id #'asm-syntax))
                          (for-each $rec
                            (syntax-flatten
                              (($lookup #'$id #'asm-syntax) $op)))))))
                  (syntax->list #'($op ...)))
                #`(lambda ($emit)
                  (run
                    #,@(reverse $defines)
                    #,@(reverse $statements))))))))))

  (define (asm-bytevector $asm)
    (lets
      ((values $port $close) (open-bytevector-output-port))
      ($emit (lambda ($u8) (put-u8 $port $u8)))
      (run ($asm $emit))
      ($close)))
)
