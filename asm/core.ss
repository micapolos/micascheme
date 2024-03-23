(library (asm core)
  (export
    define-asm-core-syntax
    define-asm-syntax
    asm
    label eq)
  (import
    (micascheme)
    (labs syntax))

  (define-aux-keyword asm-core-syntax)
  (define-aux-keyword asm-syntax)
  (define-aux-keyword label)
  (define-aux-keyword eq)

  (define-syntax-rule (define-asm-core-syntax $name $transformer)
    (begin
      (define-aux-keyword $name)
      (define-property $name asm-core-syntax $transformer)))

  (define-syntax-rule (define-asm-syntax $name $transformer)
    (begin
      (define-aux-keyword $name)
      (define-property $name asm-syntax $transformer)))

  (define-syntax asm
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          (($asm $op ...)
            (with-implicit ($asm $emit-u8)
              (let ()
                (define $label-entries (stack))
                (define $eq-entries (stack))
                (define $statements (stack))
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
                          (for-each
                            (lambda ($statement)
                              (set! $statements (push $statements $statement)))
                            (syntax-flatten
                              (($lookup #'$id #'asm-core-syntax) $op #'$emit-u8 $org))))
                        (($id $body ...)
                          (and (identifier? #'$id) ($lookup #'$id #'asm-syntax))
                          (for-each $rec
                            (syntax-flatten
                              (($lookup #'$id #'asm-syntax) $op)))))))
                  (syntax->list #'($op ...)))
                #`(lambda ($emit-u8)
                  (let
                    (#,@(reverse $label-entries))
                    (let* (#,@(reverse $eq-entries))
                      #,@(reverse $statements)
                      (void)))))))))))
)