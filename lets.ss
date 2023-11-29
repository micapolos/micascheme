(library (lets)
  (export lets in)
  (import
    (scheme)
    (binder)
    (syntax))

  (define-aux-keyword in)

  (define-syntax lets
    (lambda ($syntax)
      (lambda (lookup)
        (syntax-case $syntax (in values rec)
          ((_ (in $monad $item ...))
            (syntax-case #'($item ...) (do)
              (($decl $decls ... $result)
                (syntax-case #'$decl ()
                  ((($name $spec ...) $expr)
                    (transform-monad #'$monad #'$expr #'$value
                      (transform-binder
                        lookup
                        #'($name $spec ...)
                        #'$value
                        #'(lets (in $monad $decls ... $result)))))
                  ((($name $spec ... . $last-id) $expr)
                    (transform-monad #'$monad #'$expr #'$value
                      (transform-binder
                        lookup
                        #'($name $spec ... . $last-id)
                        #'$value
                        #'(lets (in $monad $decls ... $result)))))
                  (($id $expr)
                    (transform-monad #'$monad #'$expr #'$value
                      #'(let (($id $value))
                        (lets (in $monad $decls ... $result)))))))
              (((do $result))
                #'(lets (in $monad $result)))
              (($result)
                #'$result)))
          ((_ ((values $id ...) $expr) $decls ... $result)
            #'(let-values ((($id ...) $expr))
              (lets $decls ... $result)))
          ((_ ($id (rec $expr)) $decls ... $result)
            #'(letrec (($id $expr))
              (lets (in $monad $decls ... $result))))
          ((_ $item ...)
            #'(lets (in #f $item ...)))))))
)
