(library (function)
  (export function)
  (import
    (scheme)
    (generate)
    (binder)
    (lets)
    (syntax)
    (switch))

  (define-syntax function
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ ($name $decl ... . $tail-decl) $body)
            (identifier? #'$name)
            (lets
              (binding
                (lambda ($decl)
                  (switch $decl
                    ((identifier? $identifier)
                      (cons $identifier #f))
                    ((else $other)
                      (cons (generate-temporary) $other)))))
              ($bindings
                (map binding (syntax->list #'($decl ...))))
              ($tail-binding
                (switch #'$tail-decl
                  ((syntax-null? $null)
                    (cons $null #f))
                  ((else $other)
                    (binding $other))))
              #`(define $name
                (lambda (#,@(map car $bindings) . #,(car $tail-binding))
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
                    (cons $tail-binding (reverse $bindings)))))))))))
)
