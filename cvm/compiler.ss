(library (cvm compiler)
  (export
    op
    define-op
    compile-cvm)
  (import (micascheme))

  (define-rule-syntax (op ($param ...) $body ...)
    (lambda ($param ...)
      (syntax-case #`(#,$param ...) ()
        (($param ...) #`(begin $body ...)))))

  (define-syntax define-op
    (syntax-rules ()
      ((_ ($id $param ...) $op)
        (define-op $id
          (lambda ($syntax)
            (syntax-case $syntax ()
              ((_ $param ...) $op)))))
      ((_ $id $op)
        (begin
          (define-aux-keyword $id)
          (define-property $id op $op)))))

  (define-syntax (compile-cvm $syntax $lookup)
    (syntax-case $syntax ()
      ((_ $op ...)
        #`(lambda ($bytevector)
          (run-void
            (define $sp (bytevector-length $bytevector))
            #,@(map
              (rec parse-op
                (lambda ($op)
                  (syntax-case $op ()
                    (($id $arg ...)
                      (and
                        (identifier? #'$id)
                        ($lookup #'$id #'op))
                      (app
                        (app ($lookup #'$id #'op) $op)
                        #'$bytevector #'$sp)))))
              (syntax->list #'($op ...))))))))
)
