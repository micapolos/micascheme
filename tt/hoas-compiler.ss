(library (tt hoas-compiler)
  (export
    type?
    type=?

    typed
    typed?
    typed-type
    typed-ref

    compile-type
    compile-typed

    typed->datum)
  (import
    (scheme)
    (data)
    (union)
    (lets)
    (list)
    (procedure)
    (switch)
    (tt hoas)
    (tt lookup)
    (prefix (tt keywords) %))

  (define type? term?)

  (define (type=? $lhs $rhs)
    (term=? (lambda ($depth $lhs $rhs) (equal? $lhs $rhs)) 0 $lhs $rhs))

  (data (typed type ref))

  (define (typed->datum $typed)
    `(typed
      ,(term->datum
        (lambda ($depth $obj) $obj)
        0
        (typed-type $typed))
      ,(switch (typed-ref $typed)
        ((type? $type)
          (term->datum
            (lambda ($depth $obj) $obj)
            0
            $type))
        ((else $syntax)
          (syntax->datum $syntax)))))

  (define (compile-identifier $syntax)
    (switch $syntax
      ((identifier? $identifier) $identifier)
      ((else $other) (syntax-error $other "not identifier"))))

  (define (compile-nonnegative-integer $syntax)
    (lets
      ($datum (syntax->datum $syntax))
      (cond
        ((and (integer? $datum) (nonnegative? $datum)) $datum)
        (else (syntax-error $syntax "not nonnegative integer")))))

  (define (compile-arrow-results $lookup $syntax)
    (syntax-case $syntax (%values %void)
      ((%values xs ...)
        (map (partial compile-type $lookup) #'(xs ...)))
      (%void
        (list))
      (x
        (list (compile-type $lookup #'x)))))

  (define (compile-type $lookup $syntax)
    (syntax-case $syntax (%type %pi %lambda %quote)
      (id
        (lets
          ($datum (datum id))
          (or
            (boolean? $datum)
            (number? $datum)
            (char? $datum)
            (string? $datum)))
        (native (datum id)))
      (id
        (and
          (identifier? #'id)
          (type? ($lookup #'id)))
        ($lookup #'id))
      ((%quote id)
        (native (datum id)))
      (%type
        (universe 0))
      ((%type n)
        (universe (compile-nonnegative-integer #'n)))
      ((%lambda x)
        (compile-type $lookup #'x))
      ((%lambda id ids ... x)
        (abstraction
          (lambda ($arg)
            (lets
              ($identifier (compile-identifier #'id))
              (compile-type
                (lookup-push free-identifier=? $lookup #'id $arg)
                #'(%lambda ids ... x))))))
      ((%pi param ... result)
        (fold-right
          arrow
          (compile-type $lookup #'result)
          (map (partial compile-type $lookup) #'(param ...))))
      ((lhs rhs ...)
        (fold-left
          term-apply
          (compile-type $lookup #'lhs)
          (map (partial compile-type $lookup) #'(rhs ...))))
      (x
        (syntax-error #'x "not type"))))

  (define (compile-typed $lookup $syntax)
    (syntax-case $syntax (%typed %type %lambda)
      (n
        (number? (datum n))
        (typed ($lookup #'number) #'n))
      (id
        (and
          (identifier? #'id)
          (typed? ($lookup #'id)))
        ($lookup #'id))
      ((%typed t x)
        (typed
          (compile-type $lookup #'t)
          #'x))
      ((%type t)
        (typed
          (universe 0)
          (compile-type $lookup #'t)))
      ((%lambda body)
        (compile-typed $lookup #'body))
      ((%lambda (id t) param* ... body)
        (lets
          ($param-type (compile-type $lookup #'t))
          ($typed-body
            (compile-typed
              (lookup-push free-identifier=? $lookup #'id (typed $param-type #'id))
              #'(%lambda param* ... body)))
          (typed
            (arrow $param-type (typed-type $typed-body))
            #`(lambda (id)
              #,(typed-ref $typed-body)))))
      (other
        (syntax-error #'other "not typed"))))
)
