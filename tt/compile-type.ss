(library (tt compile-type)
  (export compile-type void type vararg forall)
  (import
    (scheme)
    (procedure)
    (lets)
    (switch)
    (syntax)
    (prefix (tt keywords) %)
    (tt lookup)
    (tt type))

  (define-keywords type forall vararg)

  (define (compile-values $lookup $syntax)
    (syntax-case $syntax (%values %void)
      ((%values xs ...)
        (map (partial compile-type $lookup) #'(xs ...)))
      (%void
        (list))
      (x
        (list (compile-type $lookup #'x)))))

  (define (compile-type $lookup $syntax)
    (syntax-case $syntax (%type %forall %lambda %...)
      (%type type-type)
      ((%forall param ... result)
        (forall-type
          (length #'(param ...))
          (lambda $args
            (compile-type
              (lookup-push* free-identifier=? $lookup #'(param ...) $args)
              #'result))))
      ((%lambda params ... param %... values)
        (lambda-type
          (append
            (map (partial compile-type $lookup) #'(params ...))
            (compile-type $lookup #'param))
          (compile-values $lookup #'values)))
      ((%lambda params ... values)
        (lambda-type
          (map (partial compile-type $lookup) #'(params ...))
          (compile-values $lookup #'values)))
      ((id arg args ...)
        (switch ($lookup #'id)
          ((type-declaration? $type-declaration)
            (cond
              ((= (type-declaration-arity $type-declaration) (+ 1 (length #'(args ...))))
                (declared-type $type-declaration
                  (map (partial compile-type $lookup) #'(arg args ...))))
              (else
                (syntax-error $syntax "invalid type arity"))))
          ((else $other)
            (syntax-error #'id "invalid type"))))
      (id
        (identifier? #'id)
        (switch ($lookup #'id)
          ((type? $type) $type)
          ((type-declaration? $type-declaration)
            (cond
              ((zero? (type-declaration-arity $type-declaration))
                (declared-type $type-declaration (list)))
              (else
                (syntax-error $syntax "invalid type arity"))))
          ((else $other)
            (syntax-error #'id "invalid type"))))))
)
