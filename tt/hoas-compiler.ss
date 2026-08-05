(library (tt hoas-compiler)
  (export
    type
    type?
    type=?

    typed
    typed?
    typed-type
    typed-ref

    macro
    macro?
    macro-procedure

    transformer
    transformer?
    transformer-procedure

    compile-type
    compile-identifier
    compile-typed
    compile-value
    compile-typeof
    compile-valueof
    compile-define
    compile-define-class
    compile-define-macro
    compile-define-syntax
    compile-instantiated-lambda
    compile-unified-args

    boolean-type
    number-type
    char-type
    string-type
    datum-type

    typed->datum
    typed->syntax)
  (import
    (scheme)
    (data)
    (lets)
    (list)
    (procedure)
    (switch)
    (system)
    (syntax)
    (identifier)
    (boolean)
    (pair)
    (list)
    (list-syntax)
    (number)
    (tt hoas)
    (tt lookup)
    (tt primitive)
    (tt type)
    (prefix (tt keywords) %))

  (data (typed type ref))
  (data (macro procedure))
  (data (transformer procedure))
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
  (define lookup-transformer? (partial lookup? transformer? #'transformer))

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
    (syntax-case $syntax (%type %pi %forall %quote %boolean %number %char %string %datum %tuple %union %...)
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
      ((%tuple t ...)
        (tuple (map (partial compile-type $lookup) #'(t ...))))
      ((%union t ...)
        (union (map (partial compile-type $lookup) #'(t ...))))
      ((%forall () x)
        (compile-type $lookup #'x))
      ((%forall (id ids ...) x)
        (abstraction
          (lambda ($arg)
            (lets
              ($identifier (compile-identifier #'id))
              (compile-type
                (lookup-push $lookup #'id $arg)
                #'(%forall (ids ...) x))))))
      ((%pi (param* ... param %...) result)
        (arrow
          (map (partial compile-type $lookup) #'(param* ...))
          (compile-type $lookup #'param)
          (compile-type $lookup #'result)))
      ((%pi (param* ...) result)
        (arrow
          (map (partial compile-type $lookup) #'(param* ...))
          #f
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

  (define (compile-unified-typed $lookup $syntax $compile-unified)
    (lets
      ((typed $type $value)
        (compile-typed $lookup $syntax))
      ((values $subst $type)
        (type-instantiate $type))
      ((unified $subst $typed)
        ($compile-unified (unified $subst (typed $type $value))))
      (typed
        (type-finalize $subst (typed-type $typed))
        (typed-ref $typed))))

  (define (compile-instantiated-lambda $lookup $syntax)
    (lets
      ($typed (compile-typed $lookup $syntax))
      ((values $subst $type)
        (type-instantiate (typed-type $typed)))
      (switch $type
        ((arrow? $arrow)
          (values
            $subst
            (typed
              $arrow
              (typed-ref $typed))))
        ((else $not-arrow)
          (syntax-error $syntax
            (format "invalid type ~s, expected pi, in"
              (type->datum $type)))))))

  (define (compile-unified-value $lookup $subst $expected-type $syntax)
    (lets
      ((typed $type $value) (compile-typed $lookup $syntax))
      (switch (type-unify $subst $expected-type $type)
        ((false? _)
          (syntax-error $syntax
            (format "invalid type ~s, expected ~s, in"
              (type->datum (type-finalize $subst $type))
              (type->datum (type-finalize $subst $expected-type)))))
        ((else $subst)
          (unified $subst $value)))))

  (define (cons-compiled-unified-value $lookup $unified-values $type $syntax)
    (lets
      ((unified $subst $values) $unified-values)
      ((unified $subst $value)
        (compile-unified-value $lookup $subst $type $syntax))
      (unified $subst (cons $value $values))))

  (define (cons-compiled-unified-values $lookup $unified-values $types $syntaxes)
    (fold-left
      (partial cons-compiled-unified-value $lookup)
      $unified-values
      $types
      $syntaxes))

  (define (compile-unified-args $lookup $app-syntax $subst $types $type...? $syntaxes)
    (unified-map reverse
      (switch $type...?
        ((false? _)
          (cond
            ((= (length $types) (length $syntaxes))
              (cons-compiled-unified-values $lookup
                (unified $subst (list))
                $types
                $syntaxes))
            (else
              (syntax-error $app-syntax
                (format "invalid argument count ~s, expected ~s, in"
                  (length $syntaxes)
                  (length $types))))))
        ((else $type...)
          (cond
            ((<= (length $types) (length $syntaxes))
              (lets
                ((values $syntaxes $syntaxes...)
                  (split $syntaxes (length $types)))
                ($unified-values
                  (cons-compiled-unified-values $lookup
                    (unified $subst (list))
                    $types
                    $syntaxes))
                (cons-compiled-unified-values $lookup
                  $unified-values
                  (make-list (length $syntaxes...) $type...)
                  $syntaxes...)))
            (else
              (syntax-error $app-syntax
                (format "invalid argument count ~s, expected at least ~s, in"
                  (length $syntaxes)
                  (length $types)))))))))

  (define (compile-arity $syntax)
    (or
      (switch? (syntax->datum $syntax)
        ((nonnegative-integer? $arity) $arity))
      (syntax-error $syntax "invalid arity")))

  (define (compile-index $arity $syntax)
    (or
      (switch? (syntax->datum $syntax)
        ((nonnegative-integer? $index)
          (and (< $index $arity) $index)))
      (syntax-error $syntax "invalid index")))

  (define (compile-typed $lookup $syntax)
    (syntax-case $syntax (%unchecked %lambda %forall %quote %if %tuple-constructor %tuple %tuple-ref %union %union-case %...)
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
          (lookup-macro? $lookup #'id))
        ((macro-procedure (lookup-macro? $lookup #'id)) $lookup $syntax))
      ((id . x)
        (and
          (identifier? #'id)
          (lookup-macro? $lookup #'id))
        ((macro-procedure (lookup-macro? $lookup #'id)) $lookup $syntax))
      (id
        (and
          (identifier? #'id)
          (lookup-transformer? $lookup #'id))
        (compile-typed $lookup
          ((transformer-procedure (lookup-transformer? $lookup #'id)) $syntax)))
      ((id . x)
        (and
          (identifier? #'id)
          (lookup-transformer? $lookup #'id))
        (compile-typed $lookup
          ((transformer-procedure (lookup-transformer? $lookup #'id)) $syntax)))
      ((%quote x)
        (typed
          datum-type
          #''x))
      ((%tuple-constructor arity)
        (lets
          ($arity (compile-arity #'arity))
          ($indices (iota $arity))
          (typed
            (arity-type $arity tuple)
            (case $arity
              ((0) #'(lambda () '()))
              ((1) #'(lambda (x) x))
              ((2) #'cons)
              (else #'vector)))))
      ((%tuple x ...)
        (lets
          ($typed-xs (map (partial compile-typed $lookup) #'(x ...)))
          (typed
            (tuple (map typed-type $typed-xs))
            (syntax-case #'(x ...) ()
              (() #''())
              ((x) #'x)
              ((x y) #'(cons x y))
              ((x ...) #'(vector x ...))))))
      ((%tuple-ref x index)
        (compile-unified-typed $lookup #'x
          (lambda ($unified-typed)
            (lets
              ((unified $subst $typed) $unified-typed)
              (switch (typed-type $typed)
                ((tuple? $tuple)
                  (switch (list-ref? (tuple-args $tuple) (datum index))
                    ((false? _)
                      (syntax-error #'index "invalid tuple index"))
                    ((else $ref-type)
                      (lets
                        ($x (typed-ref $typed))
                        (unified $subst
                          (typed $ref-type
                            (case (length (tuple-args $tuple))
                              ((1) $x)
                              ((2) #`(#,(if (zero? (datum index)) #'car #'cdr) #,$x))
                              (else #`(vector-ref #,$x index)))))))))
                ((else $other)
                  (syntax-error #'x "not tuple")))))))
      ((%union arity index x)
        (lets
          ($arity (compile-arity #'arity))
          ($index (compile-index $arity #'index))
          ((typed $x-type $x) (compile-typed $lookup #'x))
          ($indices (iota $arity))
          ($param-types
            (map-with
              ($param-index $indices)
              (cond
                ((= $param-index $index) $x-type)
                (else (hole $param-index)))))
          (typed
            (type-finalize
              (map (always #f) $indices)
              (union $param-types))
            (case (datum arity)
              ((0) #'(throw empty-tuple))
              ((1) #'identity)
              ((2) #`(lambda (v) (cons #,(literal->syntax (zero? (datum index))) x)))
              (else #`(lambda (v) (cons index x)))))))
      ((%union-case x fn ...)
        (compile-unified-typed $lookup #'x
          (lambda ($unified-typed)
            (lets
              ((unified $subst $typed) $unified-typed)
              (switch (typed-type $typed)
                ((union? $union)
                  (lets
                    ($x (typed-ref $typed))
                    (todo)))
                ((else $other)
                  (syntax-error #'x "not union")))))))
      ((%unchecked t x)
        (typed
          (compile-type $lookup #'t)
          #'x))
      ((%forall (t ...) x)
        (typed
          (compile-typeof $lookup #'(t ...) #'x)
          (compile-valueof $lookup #'(t ...) #'x)))
      ((%lambda ((id t) ... (id... t... %...)) body)
        (lets
          ($param-types (map (partial compile-type $lookup) #'(t ...)))
          ($param-type... (compile-type $lookup #'t...))
          ($typed-body
            (compile-typed
              (lookup-push
                (fold-left
                  lookup-push
                  $lookup
                  #'(id ...)
                  (map typed $param-types #'(id ...)))
                #'id...
                (typed $param-type... #'id...))
              #'body))
          (typed
            (arrow $param-types $param-type... (typed-type $typed-body))
            #`(lambda (id ... . id...)
              #,(typed-ref $typed-body)))))
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
            (arrow $param-types #f (typed-type $typed-body))
            #`(lambda (id ...)
              #,(typed-ref $typed-body)))))
      ((%if a b c)
        (lets
          ($condition (compile-value $lookup boolean-type #'a))
          ((typed $type $b) (compile-typed $lookup #'b))
          ($c (compile-value $lookup $type #'c))
          (typed
            $type
            #`(if #,$condition #,$b #,$c))))
      ((fn arg ...)
        (lets
          ((values $subst $typed-fn) (compile-instantiated-lambda $lookup #'fn))
          ((typed $arrow $fn) $typed-fn)
          ($args #'(arg ...))
          (lets
            ((unified $subst $args)
              (compile-unified-args $lookup $syntax
                $subst
                (arrow-params $arrow)
                (arrow-param...? $arrow)
                #'(arg ...)))
            (typed
              (type-finalize $subst (arrow-result $arrow))
              #`(
                #,$fn
                #,@$args)))))
      (other
        (syntax-error #'other "not typed"))))

  (define (compile-define $lookup $syntax)
    (syntax-case $syntax (%forall)
      ((_ (id (%forall t ...) param ...) body)
        (identifier? #'id)
        (compile-define $lookup
          #`(define id (%forall (t ...) (%lambda (param ...) body)))))
      ((_ (id param ...) body)
        (identifier? #'id)
        (compile-define $lookup
          #`(define id (%lambda (param ...) body))))
      ((_ id x)
        (identifier? #'id)
        (lets
          ($typed (compile-typed $lookup #'x))
          ($typed-syntax
            #`(typed
              #,(term->syntax primitive->syntax 0 (typed-type $typed))
              #'#,(typed-ref $typed)))
          #`(begin
            (define untyped #'#,(typed-ref $typed))
            #,(switch ($lookup #'id)
              ((false? _)
                #`(define-syntax id
                  (make-compile-time-value
                    #,$typed-syntax)))
              ((else _)
                #`(define-property id typed
                  #,$typed-syntax))))))))

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

  (define (compile-define-macro $syntax)
    (syntax-case $syntax ()
      ((_ id x)
        #`(define-syntax
          #,(compile-identifier #'id)
          (make-compile-time-value (macro x))))))

  (define (compile-define-syntax $syntax)
    (syntax-case $syntax ()
      ((_ id x)
        #`(define-syntax
          #,(compile-identifier #'id)
          (make-compile-time-value (transformer x))))))
)
