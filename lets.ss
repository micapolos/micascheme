(library (lets)
  (export lets)
  (import
    (scheme)
    (binder)
    (syntax)
    (procedure))

  (define-syntax lets
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax (values rec)
          ((_ ((values $id ...) $expr) $decls ... $result)
            #'(call-with-values
              (lambda () $expr)
              (lambda ($id ...)
                (lets $decls ... $result))))
          ((_ ($id (rec $expr)) $decls ... $result)
            #'(letrec (($id $expr))
              (lets $decls ... $result)))
          ((_ $decl $decls ... $result)
            (syntax-case #'$decl ()
              ((($name $spec ...) $expr)
                (transform-lets
                  $lookup
                  #'($name $spec ...)
                  #'$expr
                  #'(lets $decls ... $result)))
              ((($name $spec ... . $last-id) $expr)
                (transform-lets
                  $lookup
                  #'($name $spec ... . $last-id)
                  #'$expr
                  #'(lets $decls ... $result)))
              (($id $expr)
                (transform-binder
                  $lookup
                  #'$id
                  #'$expr
                  #'(lets $decls ... $result)))))
          ((_ $result)
            #'$result)))))
)
