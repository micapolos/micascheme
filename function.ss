(library (function)
  (export function)
  (import
    (scheme)
    (generate)
    (binder)
    (lets)
    (switch))

  (define-syntax function
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ ($name $decl ...) $body)
            (identifier? #'$name)
            (lets
              ($bindings
                (map
                  (lambda ($decl)
                    (switch $decl
                      ((identifier? $identifier)
                        (cons $identifier #f))
                      ((else $decl)
                        (cons (generate-temporary) $decl))))
                  (syntax->list #'($decl ...))))
              #`(define $name
                (lambda (#,@(map car $bindings))
                  #,(fold-left
                    (lambda ($body $binding)
                      (lets
                        ($identifier (car $binding))
                        ($decl-opt (cdr $binding))
                        (or
                          (and $decl-opt
                            (transform-binder
                              $lookup
                              $decl-opt
                              $identifier
                              $body))
                          $body)))
                    #'$body
                    (reverse $bindings))))))))))
)
