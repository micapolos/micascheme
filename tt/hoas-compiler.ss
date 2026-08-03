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
    compile-define-record
    compile-define-macro
    compile-define-syntax
    compile-print
    compile-instantiated-lambda
    compile-unified-values

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
    (pair)
    (tt hoas)
    (tt lookup)
    (tt primitive)
    (tt type)
    (prefix (tt keywords) %))

  (data (typed type ref))
  (data (macro procedure))
  (data (transformer procedure))
  (define-keyword type)

  (define boolean-declaration (generate-declaration "boolean" 0 #'boolean=? #'identity))
  (define number-declaration (generate-declaration "number" 0 #'= #'identity))
  (define char-declaration (generate-declaration "char" 0 #'char=? #'identity))
  (define string-declaration (generate-declaration "string" 0 #'string=? #'identity))
  (define datum-declaration (generate-declaration "datum" 0 #'equal? #'identity))

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
    (syntax-case $syntax (%type %pi %forall %quote %boolean %number %char %string %datum %...)
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
      ((%pi (param* ... param %...) result)
        (arrow
          (append
            (map (partial compile-type $lookup) #'(param* ...))
            (compile-type $lookup #'param))
          (compile-type $lookup #'result)))
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

  (define (compile-instantiated-lambda $lookup $syntax)
    (lets
      ($typed (compile-typed $lookup $syntax))
      ((values $subst $type) (type-instantiate (typed-type $typed)))
      (switch $type
        ((arrow? $arrow)
          (values $subst (typed $arrow (typed-ref $typed))))
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
      ((unified $subst $value) (compile-unified-value $lookup $subst $type $syntax))
      (unified $subst (cons $value $values))))

  (define (compile-unified-values $lookup $error-syntax $subst $types* $syntaxes)
    (lets
      ((unified $subst $args)
        (fold-left**
          (lambda ($unified-values $type* $syntax-box*)
            (switch $type*
              ((null? _)
                (switch $syntax-box*
                  ((null? _)
                    $unified-values)
                  ((else _)
                    (syntax-error $error-syntax "too many arguments"))))
              ((pair? $pair)
                (syntax-error $error-syntax "too little arguments"))
              ((else $type)
                (switch $syntax-box*
                  ((null/pair? $syntax-boxes)
                    (fold-left
                      (partial cons-compiled-unified-value $lookup)
                      $unified-values
                      (make-list (length $syntaxes) $type)
                      (map unbox $syntax-boxes)))
                  ((else $syntax-box)
                    (cons-compiled-unified-value $lookup $unified-values
                      $type (unbox $syntax-box)))))))
          (unified $subst (list))
          $types*
          ; box is necessary because null? returns #t both for '() and #'().
          (map box $syntaxes)))
      (unified $subst (reverse $args))))

  (define (compile-typed $lookup $syntax)
    (syntax-case $syntax (%unchecked %lambda %forall %quote %if %= %datum)
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
      ((%if a b c)
        (lets
          ($condition (compile-value $lookup boolean-type #'a))
          ((typed $type $b) (compile-typed $lookup #'b))
          ($c (compile-value $lookup $type #'c))
          (typed
            $type
            #`(if #,$condition #,$b #,$c))))
      ((%= a b)
        (lets
          ((typed $a-type $a) (compile-typed $lookup #'a))
          ((typed $b-type $b) (compile-typed $lookup #'b))
          ($type
            (or
              (type-intersect? $a-type $b-type)
              (syntax-error #'b
                (format "invalid type ~s, expected ~s, in"
                  (type->datum $b-type)
                  (type->datum $a-type)))))
          (or
            (switch $type
              ((class? $class)
                (lets
                  ($declaration (class-declaration $class))
                  (switch (declaration-arity $declaration)
                    ((zero? _)
                      (typed boolean-type
                        #`(
                          #,(declaration-eq-syntax $declaration)
                          #,$a #,$b)))
                    ((else _)
                      (syntax-error $syntax "arity no zero")))))
              ((else $not-class)
                (syntax-error $syntax
                  (format "not class ~s, in"
                    (type->datum $type))))))))
      ((%datum x)
        (lets
          ((typed $type $x) (compile-typed $lookup #'x))
          (or
            (switch $type
              ((class? $class)
                (lets
                  ($declaration (class-declaration $class))
                  (switch (declaration-arity $declaration)
                    ((zero? _)
                      (typed datum-type
                        #`(
                          #,(declaration-datum-syntax $declaration)
                          #,$x)))
                    ((else _)
                      (syntax-error $syntax "arity no zero")))))
              ((else $not-class)
                (syntax-error $syntax
                  (format "not class ~s, in"
                    (type->datum $type))))))))
      ((fn arg ...)
        (lets
          ((values $subst $typed-fn) (compile-instantiated-lambda $lookup #'fn))
          ((typed $arrow $fn) $typed-fn)
          ($args #'(arg ...))
          ($params* (arrow-params* $arrow))
          (lets
            ((unified $subst $args)
              (compile-unified-values $lookup $syntax
                $subst
                $params*
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
      ((_ id eq-syntax datum-syntax)
        (identifier? #'id)
        (compile-define-class #`(define-class (id) eq-syntax datum-syntax)))
      ((_ (id param ...) eq-syntax datum-syntax)
        (for-all identifier? #'(id param ...))
        #`(define-syntax id
          (make-compile-time-value
            (generate-declaration
              #,(literal->syntax (symbol->string (datum id)))
              #,(literal->syntax (length #'(param ...)))
              #'eq-syntax
              #'datum-syntax))))))

  (define (compile-define-record $lookup $syntax)
    (syntax-case $syntax (%= %datum)
      ((_ (id (field-id field-type) ... (%= $=) (%datum $datum)))
        (for-all identifier? #'(id field-id ...))
        (lets
          ($=id (car (generate-temporaries #'($=))))
          ($datum-id (car (generate-temporaries #'($datum))))
          ($declaration
            (generate-declaration
              (symbol->string (datum id))
              0
              $=id
              $datum-id))
          ($class (class $declaration (list)))
          ($rec-lookup (lookup-push $lookup #'id $declaration))
          ($field-types (map (partial compile-type $lookup) #'(field-type ...)))
          ($accessor-ids
            (map
              (lambda ($field-id)
                (identifier-append #'id #'id #'- $field-id))
              #'(field-id ...)))
          ($accessor-types
            (map
              (lambda ($type)
                (arrow (list $class) $type))
              $field-types))
          ($accessor-syntaxes
            (map
              (lambda ($index)
                #`(lambda ($vector)
                  (vector-ref $vector
                    #,(literal->syntax $index))))
              (iota (length $field-types))))
          ($typed-accessors
            (map typed $accessor-types $accessor-syntaxes))
          ($rec-lookup
            (fold-left lookup-push $rec-lookup
              $accessor-ids
              $typed-accessors))
          ($=syntax
            (compile-value $rec-lookup
              (arrow (list $class $class) boolean-type)
              #'$=))
          ($datum-syntax
            (compile-value $rec-lookup
              (arrow (list $class) datum-type)
              #'$datum))
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
            (define #,$=id #,$=syntax)
            (define #,$datum-id #,$datum-syntax))))))

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

  (define (compile-print $lookup $syntax)
    (syntax-case $syntax ()
      ((_ x)
        #`(pretty-print
          #,(typed-ref
            (compile-typed $lookup #'x))))))
)
