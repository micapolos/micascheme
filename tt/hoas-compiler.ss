(library (tt hoas-compiler)
  (export
    type?
    type=?

    typed
    typed?
    typed-type
    typed-ref

    macro
    macro?
    macro-procedure

    compile-type
    compile-identifier
    compile-typed
    compile-value
    compile-typeof
    compile-valueof
    compile-define
    compile-define-class
    compile-define-record
    compile-define-macro
    compile-print

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
    (syntax)
    (identifier)
    (boolean)
    (tt hoas)
    (tt lookup)
    (tt primitive)
    (tt type)
    (prefix (tt keywords) %))

  (data (typed type ref))
  (data (macro procedure))
  (define-keyword type)

  (define boolean-declaration (generate-declaration "boolean" 0))
  (define number-declaration (generate-declaration "number" 0))
  (define char-declaration (generate-declaration "char" 0))
  (define string-declaration (generate-declaration "string" 0))
  (define datum-declaration (generate-declaration "datum" 0))

  (define boolean-type (class boolean-declaration (list)))
  (define number-type (class number-declaration (list)))
  (define char-type (class char-declaration (list)))
  (define string-type (class string-declaration (list)))
  (define datum-type (class datum-declaration (list)))

  (define (lookup? $predicate? $property $lookup $id)
    (switch ($lookup $id)
      (($predicate? $x) $x)
      ((else _)
        (switch? ($lookup $id $property)
          (($predicate? $x) $x)))))

  (define lookup-declaration? (partial lookup? declaration? #'declaration))
  (define lookup-type? (partial lookup? type? #'type))
  (define lookup-typed? (partial lookup? typed? #'typed))
  (define lookup-macro? (partial lookup? macro? #'macro))

  (define (typed->datum $typed)
    `(typed
      ,(type->datum
        (typed-type $typed))
      ,(syntax->datum (typed-ref $typed))))

  (define (typed->syntax $typed)
    #`(typed
      #,(type->syntax (typed-type $typed))
      #'#,(typed-ref $typed)))

  (define (compile-identifier $syntax)
    (switch $syntax
      ((identifier? $identifier)
        $identifier)
      ((else $other)
        (syntax-error $other "not identifier"))))

  (define (compile-type $lookup $syntax)
    (syntax-case $syntax (%type %pi %forall %quote %boolean %number %char %string %datum)
      (id
        (and
          (identifier? #'id)
          (lookup-type? $lookup #'id))
        (lookup-type? $lookup #'id))
      (id
        (and
          (identifier? #'id)
          (lookup-declaration? $lookup #'id))
        (lets
          ($declaration (lookup-declaration? $lookup #'id))
          (cond
            ((= 0 (declaration-arity $declaration))
              (class $declaration (list)))
            (else
              (syntax-error #'id "declaration with arity")))))
      ((id arg arg* ...)
        (and
          (identifier? #'id)
          (lookup-declaration? $lookup #'id))
        (lets
          ($declaration (lookup-declaration? $lookup #'id))
          ($args #'(arg arg* ...))
          (cond
            ((= (length $args) (declaration-arity $declaration))
              (class $declaration
                (map (partial compile-type $lookup) $args)))
            (else
              (syntax-error #'id "invalid arity")))))
      (%boolean boolean-type)
      (%number number-type)
      (%char char-type)
      (%string string-type)
      (%datum datum-type)
      ((%forall x)
        (compile-type $lookup #'x))
      ((%forall id ids ... x)
        (abstraction
          (lambda ($arg)
            (lets
              ($identifier (compile-identifier #'id))
              (compile-type
                (lookup-push $lookup #'id $arg)
                #'(%forall ids ... x))))))
      ((%pi (param* ...) result)
        (arrow
          (map (partial compile-type $lookup) #'(param* ...))
          (compile-type $lookup #'result)))
      ((lhs rhs ...)
        (fold-left
          term-apply
          (compile-type $lookup #'lhs)
          (map (partial compile-type $lookup) #'(rhs ...))))
      (other
        (syntax-error #'other "not type"))))

  (define (compile-typeof $lookup $type-params $syntax)
    (switch $type-params
      ((null? _)
        (typed-type (compile-typed $lookup $syntax)))
      ((else $pair)
        (abstraction
          (lambda ($arg)
            (compile-typeof
              (lookup-push $lookup (car $pair) $arg)
              (cdr $pair)
              $syntax))))))

  (define (compile-valueof $lookup $type-params $syntax)
    (typed-ref
      (compile-typed
        (fold-left
          lookup-push
          $lookup
          $type-params
          (map variable (iota (length $type-params))))
        $syntax)))

  (define (compile-value $lookup $type $syntax)
    (lets
      ($typed (compile-typed $lookup $syntax))
      (cond
        ((type=? (typed-type $typed) $type)
          (typed-ref $typed))
        (else
          (syntax-error $syntax
            (format "invalid type ~s, expected ~s, in"
              (type->datum (typed-type $typed))
              (type->datum $type)))))))

  (define (compile-typed $lookup $syntax)
    (syntax-case $syntax (%unchecked %lambda %forall %quote %and)
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
          (lookup-typed? $lookup #'id))
        (lookup-typed? $lookup #'id))
      (id
        (and
          (identifier? #'id)
          (macro? ($lookup #'id)))
        ((macro-procedure ($lookup #'id)) $lookup $syntax))
      ((id . x)
        (and
          (identifier? #'id)
          (macro? ($lookup #'id)))
        ((macro-procedure ($lookup #'id)) $lookup $syntax))
      ((%quote x)
        (typed
          datum-type
          #''x))
      ((%unchecked t x)
        (typed
          (compile-type $lookup #'t)
          #'x))
      ((%forall t ... x)
        (typed
          (compile-typeof $lookup #'(t ...) #'x)
          (compile-valueof $lookup #'(t ...) #'x)))
      ((%lambda ((id t) ...) body)
        (lets
          ($param-types (map (partial compile-type $lookup) #'(t ...)))
          ($typed-body
            (compile-typed
              (fold-left
                lookup-push
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
          ;(run (pretty-print `(instantiated ,(type->datum $fn-type))))
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
                        (map (partial compile-typed $lookup) #'(arg ...)))
                      ;(run (pretty-print `(args ,@(map compiled->datum $typed-args))))
                      ($subst
                        (fold-left
                          (lambda ($subst $lhs $rhs $syntax)
                            ;(run (pretty-print `(unifying ,(type->datum $lhs) ,(type->datum $rhs))))
                            (or
                              (type-unify $subst $lhs $rhs)
                              (syntax-error $syntax
                                (format "invalid type ~s, expected ~s, in"
                                  (type->datum $rhs)
                                  ; TODO: $lhs needs to be generalized before printing!
                                  (type->datum (type-subst-apply $subst $lhs))))))
                          $subst
                          (arrow-params $arrow)
                          (map typed-type $typed-args)
                          $args))
                      ;(run (pretty-print (type-subst->datum $subst)))
                      ($arrow (type-subst-apply $subst $arrow))
                      ;(run (pretty-print `(substituted ,(type->datum $arrow))))
                      ($holes (type-holes $arrow))
                      ;(run (pretty-print `(holes ,@$holes)))
                      ($result-type (fold-left type-generalize (arrow-result $arrow) $holes))
                      ;(run (pretty-print `(result ,(type->datum $result-type))))
                      (typed
                        (fold-left type-generalize (arrow-result $arrow) $holes)
                        #`(
                          #,(typed-ref $typed-fn)
                          #,@(map typed-ref $typed-args))))))))
            ((else $other)
              (syntax-error #'fn "not function")))))
      (other
        (syntax-error #'other "not typed"))))

  (define (compile-define $lookup $syntax)
    (syntax-case $syntax (%forall)
      ((_ (id (%forall t ...) param ...) body)
        (identifier? #'id)
        (compile-define $lookup
          #`(define id (%forall t ... (%lambda (param ...) body)))))
      ((_ (id param ...) body)
        (identifier? #'id)
        (compile-define $lookup
          #`(define id (%lambda (param ...) body))))
      ((_ id x)
        (identifier? #'id)
        (lets
          ($typed (compile-typed $lookup #'x))
          #`(begin
            (define untyped #'#,(typed-ref $typed))
            (define-syntax id
              (make-compile-time-value
                (typed
                  #,(term->syntax primitive->syntax 0 (typed-type $typed))
                  #'#,(typed-ref $typed)))))))))

  (define (compile-define-class $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (identifier? #'id)
        (compile-define-class #`(define-class (id))))
      ((_ (id param ...))
        (for-all identifier? #'(id param ...))
        #`(define-syntax id
          (make-compile-time-value
            (generate-declaration
              #,(literal->syntax (symbol->string (datum id)))
              #,(literal->syntax (length #'(param ...)))))))))

  (define (compile-define-record $lookup $syntax)
    (syntax-case $syntax ()
      ((_ (id (field-id field-type) ...))
        (for-all identifier? #'(id field-id ...))
        (lets
          ($declaration
            (generate-declaration
              (symbol->string (datum id))
              0))
          ($field-types (map (partial compile-type $lookup) #'(field-type ...)))
          ($field-datum-ids
            (map
              (lambda ($type)
                (or
                  (switch? $type
                    ((class? $class)
                      (and
                        (zero? (declaration-arity (class-declaration $class)))
                        (identifier-append #'id
                          (datum->syntax #'id
                            (string->symbol
                              (symbol->string
                                (declaration-id (class-declaration $class)))))
                          #'->datum))))
                  (syntax-error #'id "no datum")))
              $field-types))
          ($field-equal-ids
            (map
              (lambda ($type)
                (or
                  (switch? $type
                    ((class? $class)
                      (and
                        (zero? (declaration-arity (class-declaration $class)))
                        (identifier-append #'id
                          (datum->syntax #'id
                            (string->symbol
                              (symbol->string
                                (declaration-id (class-declaration $class)))))
                          #'=?))))
                  (syntax-error #'id "no datum")))
              $field-types))
          #`(begin
            (define-keyword id)
            (define-property id declaration
              #,(declaration->syntax $declaration))
            (define-property id typed
              #,(typed->syntax
                (typed
                  (arrow $field-types (class $declaration (list)))
                  #'vector)))
            #,@(map
              (lambda ($index $id $type)
                #`(define-syntax
                  #,(identifier-append #'id #'id #'- $id)
                  (make-compile-time-value
                    #,(typed->syntax
                      (typed
                        (arrow (list (class $declaration (list))) $type)
                        #`(lambda ($vector)
                          (vector-ref $vector #,(literal->syntax $index))))))))
              (iota (length $field-types))
              #'(field-id ...)
              $field-types)
            (define-syntax
              #,(identifier-append #'id #'id #'->datum)
              (make-compile-time-value
                #,(typed->syntax
                  (typed
                    (arrow (list (class $declaration (list))) datum-type)
                    #`(lambda ($vector)
                      `(id
                        ,#,@(map
                          (lambda ($index $field-datum-id)
                            #`(
                              #,(typed-ref
                                (or
                                  (lookup-typed? $lookup $field-datum-id)
                                  (syntax-error $field-datum-id "dupcia")))
                              (vector-ref $vector #,(literal->syntax $index))))
                          (iota (length $field-datum-ids))
                          $field-datum-ids)))))))
            (define-syntax
              #,(identifier-append #'id #'id #'=?)
              (make-compile-time-value
                #,(typed->syntax
                  (typed
                    (arrow
                      (list
                        (class $declaration (list))
                        (class $declaration (list)))
                      boolean-type)
                    #`(lambda ($lhs $rhs)
                      (and
                        #,@(map
                          (lambda ($index $field-equal-id)
                            #`(
                              #,(typed-ref
                                (or
                                  (lookup-typed? $lookup $field-equal-id)
                                  (syntax-error $field-equal-id "dupcia")))
                              (vector-ref $lhs #,(literal->syntax $index))
                              (vector-ref $rhs #,(literal->syntax $index))))
                          (iota (length $field-datum-ids))
                          $field-equal-ids))))))))))))

  (define (compile-define-macro $syntax)
    (syntax-case $syntax ()
      ((_ id x)
        #`(define-syntax
          #,(compile-identifier #'id)
          (make-compile-time-value (macro x))))))

  (define (compile-print $lookup $syntax)
    (syntax-case $syntax ()
      ((_ x)
        #`(pretty-print
          #,(typed-ref
            (compile-typed $lookup #'x))))))
)
