(library (tt hoas-compiler)
  (export
    type?
    type=?

    typed
    typed?
    typed-type
    typed-ref

    compile-type)
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
    (syntax-case $syntax (%type %forall %lambda %quote)
      (id
        (number? (datum id))
        (native (datum id)))
      ((%quote id)
        (native (datum id)))
      (id
        (and
          (identifier? #'id)
          (type? ($lookup #'id)))
        ($lookup #'id))
      (%type
        (universe 0))
      ((%type n)
        (universe (compile-nonnegative-integer #'n)))
      ((%forall x)
        (compile-type $lookup #'x))
      ((%forall id ids ... x)
        (abstraction
          (lambda ($arg)
            (lets
              ($identifier (compile-identifier #'id))
              (compile-type
                (lookup-push free-identifier=? $lookup #'id $arg)
                #'(%forall ids ... x))))))
      ((%lambda param ... result)
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
)
