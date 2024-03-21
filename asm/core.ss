(library (asm core)
  (export
    define-asm-core-syntax
    define-asm-core-syntax-rule
    define-asm-syntax
    define-asm-syntax-rule
    asm
    asm-bytevector
    label eq)
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
  (define-aux-keyword ds)
  (define-aux-keyword eq)

  (define-syntax asm
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          (($asm $op ...)
            (with-implicit ($asm $emit)
              (let ()
                (define $label-entries (stack))
                (define $eq-entries (stack))
                (define $statements (stack))
                (define (push-statement! $statement)
                  (set! $statements (push $statements $statement)))
                (define (size? $datum)
                  (and (integer? $datum) (nonnegative? $datum)))
                (define $org (make-parameter 0))
                (for-each
                  (rec $rec
                    (lambda ($op)
                      (syntax-case $op (eq label)
                        ((eq $name $expr)
                          (identifier? #'$name)
                          (set! $eq-entries (push $eq-entries #'($name $expr))))
                        ((label $name) (identifier? #'$name)
                          (set! $label-entries (push $label-entries #`($name #,($org)))))
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
                  (let
                    (#,@(reverse $label-entries))
                    (let* (#,@(reverse $eq-entries))
                      #,@(reverse $statements)
                      (void)))))))))))

  (define (asm-bytevector $asm)
    (lets
      ((values $port $close) (open-bytevector-output-port))
      ($emit (lambda ($u8) (put-u8 $port $u8)))
      (run ($asm $emit))
      ($close)))
)
