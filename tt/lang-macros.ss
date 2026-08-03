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
          ($params* (arrow-params* $arrow))
          (cond
            ; TODO: varargs
            ((> (length $args) (length $params*))
              (syntax-error $syntax "invalid arity"))
            (else
              (lets
                ((unified $subst $args)
                  (compile-unified-values $lookup $subst
                    (list-take $params* (length $args))
                    $args))
                (typed
                  (type-finalize $subst
                    (arrow
                      (list-drop $params* (length $args))
                      (arrow-result $arrow)))
                  #`(partial
                    #,$fn
                    #,@$args)))))))))
)
