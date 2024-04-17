(library (lets)
  (export lets)
  (import
    (scheme)
    (binder)
    (syntax)
    (procedure))

  (define-lookup-syntax (lets $syntax $lookup)
    (syntax-case $syntax (values lambda rec run)
      ((_ ((values $id ...) $expr) $decls ... $result)
        #'(call-with-values
          (lambda () $expr)
          (lambda ($id ...)
            (lets $decls ... $result))))
      ((_ (lambda ($name $param ...) $body ...) $decls ... $result)
        #'(lets
          ($name (lambda ($param ...) $body ...))
          $decls ...
          $result))
      ((_ (lambda ($name $param ... . $rest) $body ...) $decls ... $result)
        #'(lets
          ($name (lambda ($param ... . $rest) $body ...))
          $decls ...
          $result))
      ((_ ($id (rec $expr)) $decls ... $result)
        #'(letrec (($id $expr))
          (lets $decls ... $result)))
      ((_ ($name $expr) $decls ... $result)
        (transform-binder
          $lookup
          #'$name
          #'$expr
          #'(lets $decls ... $result)))
      ((_ $result)
        #'$result)))
)
