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

    boolean-type
    number-type
    char-type
    string-type

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
    (tt primitive)
    (prefix (tt keywords) %))

  (define type? term?)

  (define (type=? $lhs $rhs)
    (term=? primitive=? 0 $lhs $rhs))

  (data (typed type ref))

  (define boolean-type (generate-class "boolean"))
  (define number-type (generate-class "number"))
  (define char-type (generate-class "char"))
  (define string-type (generate-class "string"))

  (define (typed->datum $typed)
    `(typed
      ,(term->datum
        primitive->datum
        0
        (typed-type $typed))
      ,(switch (typed-ref $typed)
        ((type? $type)
          (term->datum
            primitive->datum
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
    (syntax-case $syntax (%type %pi %lambda %quote %boolean %number %char %string)
      (id
        (lets
          ($datum (datum id))
          (or
            (boolean? $datum)
            (number? $datum)
            (char? $datum)
            (string? $datum)))
        (atomic #'id (datum id)))
      (id
        (and
          (identifier? #'id)
          (type? ($lookup #'id)))
        ($lookup #'id))
      (%boolean boolean-type)
      (%number number-type)
      (%char char-type)
      (%string string-type)
      ((%quote id)
        (atomic #''id (datum id)))
      (%type universe)
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
        (arrow
          (map (partial compile-type $lookup) #'(param ...))
          (compile-type $lookup #'result)))
      ((lhs rhs ...)
        (fold-left
          term-apply
          (compile-type $lookup #'lhs)
          (map (partial compile-type $lookup) #'(rhs ...))))
      (other
        (syntax-error #'other "not type"))))

  (define (compile-value $lookup $type $syntax)
    (lets
      ($typed (compile-typed $lookup $syntax))
      (cond
        ((type=? (typed-type $typed) $type)
          (typed-ref $typed))
        (else
          (syntax-error $syntax "invalid type")))))

  (define (compile-typed $lookup $syntax)
    (syntax-case $syntax (%typed %type %lambda)
      (n
        (boolean? (datum n))
        (typed boolean-type #'n))
      (n
        (number? (datum n))
        (typed number-type #'n))
      (n
        (char? (datum n))
        (typed char-type #'n))
      (n
        (string? (datum n))
        (typed string-type #'n))
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
          universe
          (compile-type $lookup #'t)))
      ((%lambda (id t) ... body)
        (lets
          ($param-types (map (partial compile-type $lookup) #'(t ...)))
          ($typed-body
            (compile-typed
              (fold-left
                (partial lookup-push free-identifier=?)
                $lookup
                #'(id ...)
                (map typed $param-types #'(id ...)))
              #'body))
          (typed
            (arrow $param-types (typed-type $typed-body))
            #`(lambda (id ...)
              #,(typed-ref $typed-body)))))
      ((fn arg ...)
        (lets
          ($typed-fn (compile-typed $lookup #'fn))
          (switch (typed-type $typed-fn)
            ((arrow? $arrow)
              (lets
                ($args #'(arg ...))
                ($params (arrow-params $arrow))
                (cond
                  ((not (= (length $args) (length $params)))
                    (syntax-error $syntax "invalid arity"))
                  (else
                    (typed
                      (arrow-result $arrow)
                      #`(
                        #,(typed-ref $typed-fn)
                        #,@(map (partial compile-value $lookup)
                          (arrow-params $arrow)
                          #'(arg ...))))))))
            ((else $other)
              (syntax-error #'fn "not lambda")))))
      (other
        (syntax-error #'other "not typed"))))
)
