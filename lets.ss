(library (lets)
  (export lets)
  (import
    (scheme)
    (binder))

  (define-syntax lets
    (lambda ($syntax)
      (lambda (lookup)
        (syntax-case $syntax (do)
          ((_ $decl $decls ... $result)
            (syntax-case #`$decl (do rec values)
              (((values $id ...) $expr)
                #`(let-values ((($id ...) $expr))
                  (lets $decls ... $result)))
              ((($name $spec ...) $expr)
                (transform-binder
                  lookup
                  #'($name $spec ...)
                  #'$expr
                  #'(lets $decls ... $result)))
              ((($name $spec ... . $last-id) $expr)
                (transform-binder
                  lookup
                  #'($name $spec ... . $last-id)
                  #'$expr
                  #'(lets $decls ... $result)))
              (($id (rec $expr))
                #`(letrec (($id $expr))
                  (lets $decls ... $result)))
              (($id $expr)
                #`(let (($id $expr))
                  (lets $decls ... $result)))
              ($expr
                #`(begin $expr
                (lets $decls ... $result)))))
          ((_ (do $result)) #`$result)
          ((_ $result) #`$result)))))
)
