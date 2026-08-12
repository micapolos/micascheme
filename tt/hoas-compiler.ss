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

    typed-value-compiler
    typed-value-compiler?
    typed-value-compiler-procedure

    typed-value-box
    typed-value-box?
    typed-value-box-ref

    typed-syntax-box
    typed-syntax-box?
    typed-syntax-box-ref

    compile-type
    compile-typed-value
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
    typed-value->datum
    typed->syntax)
  (import
    (scheme)
    (data)
    (lets)
    (except (list) product)
    (procedure)
    (switch)
    (system)
    (syntax)
    (identifier)
    (boolean)
    (pair)
    (except (list) product)
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
  (data (typed-value-compiler procedure))
  (define-keyword type)

  (data (type-box ref))
  (data (typed-value-box ref))
  (data (typed-syntax-box ref))

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

  (define (typed-type? $obj)
    (and
      (typed? $obj)
      (type? (typed-ref $obj))))

  (define (lookup? $predicate? $property $lookup $id)
    (switch ($lookup $id)
      (($predicate? $x) $x)
      ((else _)
        (switch?
          (guard
            (exception ((syntax-violation? exception) #f))
            ($lookup $id $property))
          (($predicate? $x) $x)))))

  (define lookup-declaration? (partial lookup? declaration? #'declaration))
  (define lookup-type-box? (partial lookup? type-box? #'type-box))
  (define lookup-typed-value-box? (partial lookup? typed-value-box? #'typed-value-box))
  (define lookup-typed-syntax-box? (partial lookup? typed-syntax-box? #'typed-syntax-box))
  (define lookup-typed? (partial lookup? typed? #'typed))
  (define lookup-typed-type? (partial lookup? typed-type? #'typed-type))
  (define lookup-macro? (partial lookup? macro? #'macro))
  (define lookup-transformer? (partial lookup? transformer? #'transformer))
  (define lookup-typed-value-compiler? (partial lookup? typed-value-compiler? #'typed-value-compiler))

  (define (lookup-type? $lookup $id)
    (lets?
      ($type-box (lookup-type-box? $lookup $id))
      (type-box-ref $type-box)))

  (define (typed->datum $typed)
    `(typed
      ,(type->datum
        (typed-type $typed))
      ,(syntax->datum (typed-ref $typed))))

  (define (typed-value->datum $typed-value)
    `(typed
      ,(type->datum
        (typed-type $typed-value))
      ,(switch (typed-ref $typed-value)
        ((type? $type) (type->datum $type))
        ((else $other) $other))))

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

  (define (compile-param $lookup $syntax)
    (syntax-case $syntax ()
      (id
        (identifier? #'id)
        (values #'id (kind 0)))
      ((id t)
        (lets
          ($id (compile-identifier #'id))
          ($typed-value (compile-typed-value $lookup #'t))
          (run
            (unless
              (kind? (typed-type $typed-value))
              (syntax-error #'t "not kind")))
          (values $id (typed-ref $typed-value))))
      (other
        (syntax-error #'other "invalid param"))))

  (define (compile-typed-abstraction $lookup $param $compile-body)
    (lets
      ((values $id $type) (compile-param $lookup $param))
      (typed
        (product $type
          (lambda ($arg)
            (typed-type
              ($compile-body
                (lookup-push $lookup $id
                  (typed-value-box (typed $type $arg)))))))
        (abstraction
          (lambda ($arg)
            (typed-ref
              ($compile-body
                (lookup-push $lookup $id
                  (typed-value-box (typed $type $arg))))))))))

  (define (compile-product-param $lookup $syntax)
    (syntax-case $syntax ()
      ((id t)
        (lets
          ($id (compile-identifier #'id))
          ($typed-value (compile-typed-value $lookup #'t))
          (run
            (unless
              (kind? (typed-type $typed-value))
              (syntax-error #'t "not kind")))
          (values $id (typed-ref $typed-value))))
      (other
        (syntax-error #'other "invalid param"))))

  (define (compile-typed-product $lookup $param $compile-body)
    (lets
      ((values $id $type) (compile-product-param $lookup $param))
      (typed
        (kind 0)
        (product $type
          (lambda ($arg)
            (typed-ref
              ($compile-body
                (lookup-push $lookup $id
                  (typed-value-box (typed $type $arg))))))))))

  (define (compile-typed-value-ref $lookup $syntax)
    (typed-ref (compile-typed-value $lookup $syntax)))

  (define (compile-typed-value $lookup $syntax)
    (syntax-case $syntax (%quote %typeof %tuple %choice %type %boolean %number %string %char %datum %lambda %product %pi %...)
      (b
        (boolean? (datum b))
        (typed boolean-type (datum b)))
      (n
        (number? (datum n))
        (typed number-type (datum n)))
      (ch
        (char? (datum ch))
        (typed char-type (datum ch)))
      (s
        (string? (datum s))
        (typed string-type (datum s)))
      (id
        (and
          (identifier? #'id)
          (lookup-typed-value-box? $lookup #'id))
        (typed-value-box-ref (lookup-typed-value-box? $lookup #'id)))
      ((id . x)
        (and
          (identifier? #'id)
          (lookup-typed-value-compiler? $lookup #'id))
        ((typed-value-compiler-procedure (lookup-typed-value-compiler? $lookup #'id)) $lookup $syntax))
      ((%quote x)
        (typed datum-type (datum x)))
      (%type
        (typed (kind 1) (kind 0)))
      ((%type index)
        (lets
          ($index (compile-nonnegative-integer #'index))
          (typed
            (kind (+ $index 1))
            (kind $index))))
      ((%type . _)
        (syntax-error $syntax "invalid type"))
      (%boolean (typed (kind 0) boolean-type))
      (%number (typed (kind 0) number-type))
      (%char (typed (kind 0) char-type))
      (%string (typed (kind 0) string-type))
      (%datum (typed (kind 0) datum-type))
      ((%tuple t ...)
        (typed (kind 0)
          (tuple (map (dot typed-ref (partial compile-typed-value $lookup)) #'(t ...)))))
      ((%choice t ...)
        (typed (kind 0)
          (choice (map (dot typed-ref (partial compile-typed-value $lookup)) #'(t ...)))))
      ((%typeof x)
        (lets
          ($typed-value (compile-typed-value $lookup #'x))
          (typed
            (term-type (typed-type $typed-value))
            (typed-type $typed-value))))
      ((%lambda () body)
        (compile-typed-value $lookup #'body))
      ((%lambda (param . params) body)
        (compile-typed-abstraction $lookup #'param
          (lambda ($lookup)
            (compile-typed-value $lookup
              #'(%lambda params body)))))
      ((%lambda . x)
        (syntax-error $syntax "invalid lambda"))
      ((%product () body)
        (compile-typed-value $lookup #'body))
      ((%product (param . params) body)
        (compile-typed-product $lookup #'param
          (lambda ($lookup)
            (compile-typed-value $lookup
              #'(%product params body)))))
      ((%product . x)
        (syntax-error $syntax "invalid product"))
      ((%pi (params ... param %...) result)
        (typed (kind 0)
          (arrow
            (map (partial compile-typed-value-ref $lookup) #'(params ...))
            (compile-typed-value-ref $lookup #'param)
            (compile-typed-value-ref $lookup #'result))))
      ((%pi (params ...) result)
        (typed (kind 0)
          (arrow
            (map (partial compile-typed-value-ref $lookup) #'(params ...))
            #f
            (compile-typed-value-ref $lookup #'result))))
      ((%pi . x)
        (syntax-error $syntax "invalid pi"))
      ((fn arg ...)
        (fold-left
          (lambda ($typed-type $arg-syntax)
            (lets
              ((values $subst $type) (type-instantiate (typed-type $typed-type)))
              (switch $type
                ((product? $product)
                  (lets
                    ((unified $subst $arg)
                      (compile-unified-typed-type-ref
                        $lookup
                        $subst
                        (product-param $product)
                        $arg-syntax))
                    (typed
                      (type-finalize $subst (product-apply $product $arg))
                      (abstraction-apply (typed-ref $typed-type) $arg))))
                ((else $other)
                  (syntax-error $arg-syntax "can not apply")))))
          (compile-typed-value $lookup #'fn)
          #'(arg ...)))
      (other
        (syntax-error #'other "not typed"))))

  (define (compile-type $lookup $syntax)
    (syntax-case $syntax (%type %typeof %pi %lambda %quote %boolean %number %char %string %datum %tuple %choice %...)
      (n
        (number? (datum n))
        (datum n))
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
      ((%choice t ...)
        (choice (map (partial compile-type $lookup) #'(t ...))))
      ((%typeof x)
        (typed-type (compile-typed $lookup #'x)))
      ((%lambda () x)
        (compile-type $lookup #'x))
      ((%lambda (id ids ...) x)
        (abstraction
          (lambda ($arg)
            (lets
              ($identifier (compile-identifier #'id))
              (compile-type
                (lookup-push $lookup #'id (type-box $arg))
                #'(%lambda (ids ...) x))))))
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
              (lookup-push $lookup (car $pair) (type-box $arg))
              (cdr $pair)
              $syntax))))))

  (define (compile-valueof $lookup $type-params $syntax)
    (typed-ref
      (compile-typed
        (fold-left
          lookup-push
          $lookup
          $type-params
          (map type-box (map variable (iota (length $type-params)))))
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

  (define (compile-unified-typed-type-ref $lookup $subst $expected-type $syntax)
    (lets
      ((typed $type $value) (compile-typed-value $lookup $syntax))
      (switch (type-unify $subst $expected-type $type)
        ((false? _)
          (syntax-error $syntax
            (format "invalid type ~s, expected ~s, in"
              (type->datum (type-finalize $subst $type))
              (type->datum (type-finalize $subst $expected-type)))))
        ((else $subst)
          (unified $subst $value)))))

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

  (define (compile-nonnegative-integer $syntax)
    (or
      (switch? (syntax->datum $syntax)
        ((nonnegative-integer? $int) $int))
      (syntax-error $syntax "not nonnegative integer")))

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
    (syntax-case $syntax
      (
        %unchecked %lambda %forall %quote %if %is? %time
        %tuple-constructor %tuple-accessor
        %choice-constructor %choice-matcher
        %...)
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
          (lookup-typed-syntax-box? $lookup #'id))
        (typed-syntax-box-ref (lookup-typed-syntax-box? $lookup #'id)))
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
      ((%is? t x)
        (typed boolean-type
          (type=?
            (compile-type $lookup #'t)
            (typed-type (compile-typed $lookup #'x)))))
      ((%quote x)
        (typed
          datum-type
          #''x))
      ((%time x)
        (lets
          ($typed (compile-typed $lookup #'x))
          (typed
            (typed-type $typed)
            #`(time #,(typed-ref $typed)))))
      ((%tuple-constructor arity)
        (lets
          ($arity (compile-arity #'arity))
          ($indices (iota $arity))
          (typed
            (arity-type $arity
              (lambda ($args)
                (arrow $args #f (tuple $args))))
            (case $arity
              ((0) #'(lambda () '()))
              ((1) #'(lambda (x) x))
              ((2) #'cons)
              (else #'vector)))))
      ((%tuple-accessor arity index)
        (lets
          ($arity (compile-arity #'arity))
          ($index (compile-index $arity #'index))
          (typed
            (arity-type $arity
              (lambda ($args)
                (arrow (list (tuple $args)) #f (list-ref $args $index))))
            (case $arity
              ((1) #'(lambda (x) x))
              ((2) (if (zero? $index) #'car #'cdr))
              (else #'(lambda (x) (vector-ref x index)))))))
      ((%choice-constructor arity index)
        (lets
          ($arity (compile-arity #'arity))
          ($index (compile-index $arity #'index))
          (typed
            (arity-type $arity
              (lambda ($args)
                (arrow
                  (list (list-ref $args $index))
                  #f
                  (choice $args))))
            (case $arity
              ((1) #'(lambda (x) x))
              ((2) #`(lambda (x) (cons #,(literal->syntax (zero? $index)) x)))
              (else #`(lambda (x) (cons index x)))))))
      ((%choice-matcher arity)
        (lets
          ($arity (compile-arity #'arity))
          ($indices (iota $arity))
          ($tmps
            (map-with ($index $indices)
              (literal->syntax (string->symbol (string-append "f" (number->string $index))))))
          (typed
            (arity-type (+ $arity 1)
              (lambda ($args)
                (lets
                  ($result (car $args))
                  ($args (cdr $args))
                  (arrow
                    (cons
                      (choice $args)
                      (map-with
                        ($arg $args)
                        (arrow (list $arg) #f $result)))
                    #f
                    $result))))
            (case $arity
              ((1) #'(lambda (x f) (f x)))
              ((2)
                #'(lambda (x f0 f1)
                  ((if (car x) f0 f1) (cdr x))))
              (else
                #`(lambda (x #,@$tmps)
                  ((index-switch (car x) #,@$tmps) (cdr x))))))))
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
                  (map typed-syntax-box (map typed $param-types #'(id ...))))
                #'id...
                (typed-syntax-box (typed $param-type... #'id...)))
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
                (map typed-syntax-box (map typed $param-types #'(id ...))))
              #'body))
          (typed
            (arrow $param-types #f (typed-type $typed-body))
            #`(lambda (id ...)
              #,(typed-ref $typed-body)))))
      ((%lambda (rec-id rec-type) ((id type) ...) body)
        (lets
          ($result-type (compile-type $lookup #'rec-type))
          ($param-types (map (partial compile-type $lookup) #'(type ...)))
          ($arrow (arrow $param-types #f $result-type))
          ($lookup (lookup-push $lookup #'rec-id (typed-syntax-box (typed $arrow #'rec-id))))
          ($lookup (fold-left lookup-push $lookup #'(id ...) (map typed-syntax-box (map typed $param-types #'(id ...)))))
          ($body (compile-value $lookup $result-type #'body))
          (typed
            $arrow
            #`(letrec ((rec-id (lambda (id ...) #,$body))) rec-id))))
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
                    (typed-syntax-box
                      #,$typed-syntax))))
              ((else _)
                #`(define-property id typed-syntax-box
                  (typed-syntax-box
                    #,$typed-syntax)))))))))

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
