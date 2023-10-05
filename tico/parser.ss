(library (tico parser)
  (export
    context context? context-scope context-type-context-opt
    typed typed? typed-value typed-type
    empty-context
    context+type

    scope scope? scope-types
    empty-scope
    scope+type
    scope-type-ref

    parser parser? parser-context parser-args

    syntax->typed)
  (import (micascheme) (tico term) (tico type))

  (data (typed value type))
  (data (scope types))
  (data (context scope type-context-opt))

  (data (parser context args))

  ; (define (typed-resolve $typed $args)
  ;   (switch (typed-type $typed)
  ;     ((function-type? $function-type)
  ;       (map typed-cast-opt
  ;         $args
  ;         (function-type-params $function-type)))
  ;     ((else $other) #f)))

  (define (empty-scope) (scope (stack)))

  (define (scope+type $scope $type)
    (scope
      (push
        (scope-types $scope)
        $type)))

  (define (scope-type-ref $scope $type)
    (lets
      ($index
        (find-index
          (partial type-matches? $type)
          (scope-types $scope)))
      (and $index (typed (variable $index) $type))))

  (define (empty-context)
    (context (empty-scope) #f))

  (define (context+type $context $type)
    (context
      (scope+type (context-scope $context) $type)
      (context-type-context-opt $context)))

  (define (parser+arg $parser $arg)
    (parser
      (parser-context $parser)
      (push (parser-args $parser) $arg)))

  (define (scope-resolve-args $scope $args)
    $args)

  (define (context-syntax-list->typed $context $syntax-list)
    (or
      (single (context-syntax-list->typed-list $context $syntax-list))
      (syntax-error #`(#,@$syntax-list) "no single typed")))

  (define (context-syntax-list->typed-list $context $syntaxes)
    (reverse
      (parser-args
        (fold-left
          parser+syntax
          (parser $context (stack))
          $syntaxes))))

  (define (parser+syntax $parser $syntax)
    (lets
      ($context (parser-context $parser))
      ($scope (context-scope $context))
      ($args (parser-args $parser))
      (syntax-case $syntax ()
        (($get $body ...)
          (identifier-named? #`$get get)
          (lets
            ($type (context-syntax-list->type $context (syntax->list #`($body ...))))
            (parser $context
              (list
                (case (length $args)
                  ((0) (scope-type-ref $scope $type))
                  ((1) (typed-type-ref (car $args) $type))
                  (else (syntax-error $syntax "get not implemented on args")))))))
        (($do $body ...)
          (identifier-named? #`$do do)
          (syntax-error $syntax "not implemented"))
        ($other
          (lets
            ($arg (context-syntax->typed $context #`$other))
            (parser $context
              (case (length $args)
                ((0) (list $arg))
                (else (scope-resolve-args (context-scope $context) (push $args $arg))))))))))

  (define (context-syntaxes-bind $context $syntaxes $fn)
    (switch $syntaxes
      ((null? _) ($fn $context))
      ((else $pair)
        (context-syntax-bind $context (car $pair)
          (lambda ($context)
            (context-syntaxes-bind $context (cdr $pair) $fn))))))

  (define (context-syntax-bind $context $syntax $fn)
    (lets
      ($typed (context-syntax->typed $context $syntax))
      ($fn-typed ($fn (context+type $context (typed-type $typed))))
      (typed
        (application
          (function 1 (typed-value $fn-typed))
          (list (typed-value $typed)))
        (typed-type $fn-typed))))

  (define (syntax->typed $syntax)
    (context-syntax->typed (empty-context) $syntax))

  (define (context-syntax->typed $context $syntax)
    (syntax-case $syntax ()
      (($native $value $type)
        (identifier-named? #`$native native)
        (typed
          (syntax->datum #`$value)
          (context-syntax->type $context #`$type)))
      (($take $item ...)
        (identifier-named? #`$take take)
        (context-syntax-list->typed $context
          (syntax->list #`($item ...))))
      (($function $param ... ($doing $body ...))
        (and
          (identifier-named? #`$function function)
          (identifier-named? #`$doing doing))
        (lets
          ($param-types (context-syntax-list->types $context (syntax->list #`($param ...))))
          ($arity (length $param-types))
          ($context (fold-left context+type $context $param-types))
          ($typed-body (context-syntax-list->typed $context (syntax->list #`($body ...))))
          (typed
            (function $arity (typed-value $typed-body))
            (function-type $param-types (typed-type $typed-body)))))
      (($name $item ...) (identifier? #`$name)
        (lets
          ($name (syntax->datum #`$name))
          ($typed-list
            (context-syntax-list->typed-list
              $context
              (syntax->list #`($item ...))))
          ($values (map typed-value $typed-list))
          ($types (map typed-type $typed-list))
          (typed
            (application `list $values)
            (struct-type $name $types))))
      ($name (identifier? #`$name)
        (typed
          (application `list (list))
          (struct-type (syntax->datum #`$name) (list))))
      ($other
        (switch (syntax->datum $syntax)
          ((boolean? $boolean) (typed $boolean (boolean-type)))
          ((number? $number) (typed $number (number-type)))
          ((string? $string) (typed $string (string-type)))
          ((else $other) (syntax-error $syntax "dupa"))))))

  ; TODO: Evaluate in type context.
  (define (context-syntax->type $context $syntax)
    (syntax-case $syntax ()
      ($boolean
        (identifier-named? #`$boolean boolean)
        (boolean-type))
      ($number
        (identifier-named? #`$number number)
        (number-type))
      ($string
        (identifier-named? #`$string string)
        (string-type))
      (($function $param ... ($giving $body ...))
        (and
          (identifier-named? #`$function function)
          (identifier-named? #`$giving giving))
        (function-type
          (context-syntax-list->types $context (syntax->list #`($param ...)))
          (context-syntax-list->type $context (syntax->list #`($body ...)))))
      (else (syntax-error $syntax "invalid type"))))

  ; TODO: Evaluate in type context.
  (define (context-syntax-list->types $context $syntax-list)
    (map (partial context-syntax->type $context) $syntax-list))

  (define (context-syntax-list->type $context $syntax-list)
    (or
      (single (context-syntax-list->types $context $syntax-list))
      (syntax-error #`(#,@$syntax-list) "no single type")))

  (define (typed-type-ref $typed $type)
    (switch (typed-type $typed)
      ((struct-type? $struct-type)
        (lets
          ($index
            (find-index
              (partial type-matches? $type)
              (struct-type-items $struct-type)))
          (and $index
            (typed
              (application `list-ref (list (typed-value $typed) $index))
              (list-ref (struct-type-items $struct-type) $index)))))
      ((else $other)
        (error `typed-type-ref "not possible"))))
)
