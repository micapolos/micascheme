(library (tico-2)
  (export
    anything anything?
    any-boolean any-boolean?
    any-number any-number?
    any-string any-string?

    typed typed? typed-value typed-type
    term term? term-expr term-constant
    constant constant? constant-value
    field field? field-name field-value
    function function? function-params function-body
    generic generic? generic-params generic-body

    syntax->typed)
  (import
    (except (micascheme) function))

  (data (anything))
  (data (any-boolean))
  (data (any-number))
  (data (any-string))
  (data (generic params body))
  (data (function params body))
  (data (field name value))

  (data (constant value))
  (data (context bindings))
  (data (term expr constant))
  (data (typed value type))
  (data (parser context typed-stack))

  (define (empty-context)
    (context (stack)))

  (define (type-matches? $type $pattern)
    (switch $pattern
      ((anything? _) #t)
      ((any-boolean? _) (any-boolean? $type))
      ((any-number? _) (any-number? $type))
      ((any-string? _) (any-string? $type))
      ((null? $null) (null? $type))
      ((pair? $pair)
        (and
          (pair? $type)
          (type-matches? (car $type) (car $pattern))
          (type-matches? (cdr $type) (cdr $pattern))))
      ((field? $field)
        (and
          (field? $type)
          (equal? (field-name $type) (field-name $pattern))
          (type-matches? (field-value $type) (field-value $pattern))))
      ((else $other)
        (equal? $type $other))))

  (define (decompile $value $type)
    (switch $type
      ((anything? _) $value)
      ((any-boolean? _) $value)
      ((any-number? _) $value)
      ((any-string? _) $value)
      ((null? _) $value)
      ((pair? $pair)
        (cons
          (decompile (car $value) (car $pair))
          (decompile (cdr $value) (cdr $pair))))
      ((field? $field)
        (field (field-name $field)
          (decompile $value (field-value $field))))
      ((else $other)
        $other)))

  (define (typed-resolve $typed $args)
    (or
      (typed-resolve-apply $typed $args)))

  (define (typed-resolve-apply $typed $args)
    (switch (typed-type $typed)
      ((function? $function)
        (and
          (for-all type-matches?
            (map typed-type $args)
            (function-params $function))
          (typed
            (term
              `(
                ,(term-expr (typed-value $typed))
                ,@(map term-expr (map typed-value $args)))
              (lets
                ($constant (term-constant (typed-value $typed)))
                ($arg-constants (map term-constant (map typed-value $args)))
                (and $constant
                  (for-all identity $arg-constants)
                  (apply
                    (constant-value $constant)
                    (map constant-value $arg-constants)))))
            (function-body $function))))
      ((else $other) #f)))

  (define (syntax->typed $syntax)
    (parser-typed
      (fold-left
        parser+syntax
        (parser (empty-context) (stack))
        (syntax->list $syntax))))

  (define (parser-typed $parser)
    (or
      (single (parser-typed-stack $parser))
      (error `parser-typed "non-single-type-stack")))

  (define (parser+syntax $parser $syntax)
    (parser
      (parser-context $parser)
      (push
        (parser-typed-stack $parser)
        (context-syntax->typed (parser-context $parser) $syntax))))

  (define (context-syntax->typed $context $syntax)
    (or
      (context-syntax->typed-opt $context $syntax)
      (syntax-error $syntax "dupa")))

  (define (context-syntax->typed-opt $context $syntax)
    (syntax-case $syntax ()
      (($name $item ...) (identifier? #`$name)
        (typed-field
          (syntax->datum #`$name)
          (map
            (partial context-syntax->typed $context)
            (syntax->list #`($item ...)))))
      ($item
        (syntax->typed-opt #`$item))
      (else #f)))

  (define (syntax->typed-opt $syntax)
    (switch (syntax->datum $syntax)
      ((boolean? $boolean)
        (typed
          (term $boolean (constant $boolean))
          (any-boolean)))
      ((number? $number)
        (typed
          (term $number (constant $number))
          (any-number)))
      ((string? $string)
        (typed
          (term $string (constant $string))
          (any-string)))
      ((else _) #f)))

  (define (typed-field $name $typed-items)
    (typed
      (map typed-value $typed-items)
      (field $name (map typed-type $typed-items))))
)
