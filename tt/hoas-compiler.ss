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
    (system)
    (tt hoas)
    (tt lookup)
    (tt primitive)
    (tt type)
    (prefix (tt keywords) %))

  (data (typed type ref))

  (define boolean-declaration (generate-declaration "boolean" 0))
  (define number-declaration (generate-declaration "number" 0))
  (define char-declaration (generate-declaration "char" 0))
  (define string-declaration (generate-declaration "string" 0))

  (define boolean-type (class boolean-declaration (list)))
  (define number-type (class number-declaration (list)))
  (define char-type (class char-declaration (list)))
  (define string-type (class string-declaration (list)))

  (define (typed->datum $typed)
    `(typed
      ,(type->datum
        (typed-type $typed))
      ,(switch (typed-type $typed)
        ((universe? _)
          (type->datum (constant-ref (typed-ref $typed))))
        ((else _)
          (syntax->datum (typed-ref $typed))))))

  (define (compile-identifier $syntax)
    (switch $syntax
      ((identifier? $identifier) $identifier)
      ((else $other) (syntax-error $other "not identifier"))))

  (define (compile-type $lookup $syntax)
    (syntax-case $syntax (%type %-> %forall %quote %boolean %number %char %string)
      (id
        (and
          (identifier? #'id)
          (type? ($lookup #'id)))
        ($lookup #'id))
      (id
        (and
          (identifier? #'id)
          (declaration? ($lookup #'id)))
        (lets
          ($declaration ($lookup #'id))
          (cond
            ((= 0 (declaration-arity $declaration))
              (class $declaration (list)))
            (else (syntax-error #'id "declaration with arity")))))
      ((id arg arg* ...)
        (and
          (identifier? #'id)
          (declaration? ($lookup #'id)))
        (lets
          ($declaration ($lookup #'id))
          ($args #'(arg arg* ...))
          (cond
            ((= (length $args) (declaration-arity $declaration))
              (class $declaration
                (map (partial compile-type $lookup) $args)))
            (else (syntax-error #'id "invalid arity")))))
      (%boolean boolean-type)
      (%number number-type)
      (%char char-type)
      (%string string-type)
      (%type universe)
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
      ((%-> result)
        (compile-type $lookup #'result))
      ((%-> param param* ... result)
        (arrow
          (map (partial compile-type $lookup) #'(param param* ...))
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

  (define (compile-typed-arg $lookup $param $syntax)
    (switch $param
      ((universe? $universe)
        (typed
          $universe
          (compile-type $lookup $syntax)))
      ((else _)
        (compile-typed $lookup $syntax))))

  (define (compile-typed $lookup $syntax)
    (syntax-case $syntax (%typed %type %=>)
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
          (constant (compile-type $lookup #'t))))
      ((%=> (id t) ... body)
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
          ;(run (pretty-print '===type-checking===))
          ;(run (pretty-print (typed->datum $typed-fn)))
          ((values $subst $fn-type) (type-instantiate (typed-type $typed-fn)))
          ;(run (pretty-print (type->datum $fn-type)))
          (switch $fn-type
            ((arrow? $arrow)
              (lets
                ($args #'(arg ...))
                ($params (arrow-params $arrow))
                (cond
                  ((not (= (length $args) (length $params)))
                    (syntax-error $syntax "invalid arity"))
                  (else
                    (lets
                      ($typed-args
                        (map
                          (partial compile-typed-arg $lookup)
                          (arrow-params $arrow)
                          #'(arg ...)))
                      ;(run (pretty-print (map typed->datum $typed-args)))
                      ($subst
                        (fold-left
                          (lambda ($subst $lhs $rhs $syntax)
                            (or
                              (type-unify $subst $lhs $rhs)
                              (syntax-error $syntax "invalid unified type")))
                          $subst
                          (arrow-params $arrow)
                          (map typed-type $typed-args)
                          $args))
                      ($arrow (type-subst-apply $subst $arrow))
                      ;(run (pretty-print (type->datum $arrow)))
                      ($holes (type-holes $arrow))
                      ;(run (pretty-print $holes))
                      ($result-type (fold-left type-generalize (arrow-result $arrow) $holes))
                      ;(run (pretty-print (type->datum $result-type)))
                      (typed
                        (fold-left type-generalize (arrow-result $arrow) $holes)
                        #`(
                          #,(typed-ref $typed-fn)
                          #,@(map typed-ref $typed-args))))))))
            ((else $other)
              (syntax-error #'fn "not lambda")))))
      (other
        (syntax-error #'other "not typed"))))
)
