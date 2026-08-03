(library (tt lang-macros)
  (export
    compile-and
    compile-or
    compile-partial)
  (import
    (scheme)
    (lets)
    (procedure)
    (list)
    (tt hoas)
    (tt primitive)
    (tt type)
    (tt hoas-compiler))

  (define (compile-and $lookup $syntax)
    (syntax-case $syntax ()
      ((_ x ...)
        (lets
          ($xs (map (partial compile-value $lookup boolean-type) #'(x ...)))
          (typed
            boolean-type
            #`(and #,@$xs))))))

  (define (compile-or $lookup $syntax)
    (syntax-case $syntax ()
      ((_ x ...)
        (lets
          ($xs (map (partial compile-value $lookup boolean-type) #'(x ...)))
          (typed
            boolean-type
            #`(or #,@$xs))))))

  (define (compile-partial $lookup $syntax)
    (syntax-case $syntax ()
      ((_ fn arg ...)
        (lets
          ((values $subst $typed-fn) (compile-instantiated-lambda $lookup #'fn))
          ((typed $arrow $fn) $typed-fn)
          ($args #'(arg ...))
          ($params (arrow-params $arrow))
          ($param...? (arrow-param...? $arrow))
          (cond
            ; TODO: varargs
            ($param...? (todo))
            ((> (length $args) (length $params))
              (syntax-error $syntax "invalid arity"))
            (else
              (lets
                ((unified $subst $args)
                  (compile-unified-args $lookup $syntax
                    $subst
                    (list-take $params (length $args))
                    #f
                    $args))
                (typed
                  (type-finalize $subst
                    (arrow
                      (list-drop $params (length $args))
                      #f
                      (arrow-result $arrow)))
                  #`(partial
                    #,$fn
                    #,@$args)))))))))
)
