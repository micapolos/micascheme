(library (tico compiler)
  (export
    constant constant? constant-symbol constant-value
    compiled compiled? compiled-constants compiled-typed-expression
    typed typed? typed-type typed-value
    expression expression? expression-datum expression-depth

    datum->compiled)
  (import
    (micascheme)
    (tico type)
    (evaluator))

  (data (globalized constants value))
  (data (constant symbol value))
  (data (binding type symbol))
  (data (context constants bindings))
  (data (args-compiler context args))

  (data (typed type value-opt))
  (data (thunk term free-variable-count))
  (data (term bindings datum))

  (data (compiler fn))
  (data (compiled constants value))

  (data (compiler-constants->compiled $compiler $constants)
    ((compiler-fn $compiler) $constants))

  (data (value-compiler $value)
    (compiler
      (lambda ($constants)
        (compiled $constants $value))))

  (data (compiler-bind $compiler $fn)
    (compiler
      (lambda ($constants)
        (lets
          ($compiled (compiler-constants->compiled constants))
          (compiler-constants->compiled
            ($fn (compiled-value $compiled))
            (compiled-constants $compiled))))))

  (data (compilers-flatten $compilers)
    (switch $compilers
      ((null? $compilers)
        (value-compiler `()))
      ((else $pair)
        (unpair $pair $compiler $compilers
          (compiler-bind $compiler
            (lambda ($value)
              (compiler-bind (compilers-flatten $compilers)
                (lambda ($values)
                  (cons $value $values)))))))))

  (define empty-context (context (stack) (stack)))

  (define (datum->compiled $datum)
    (context-syntax->compiled
      empty-context
      (datum->syntax #`+ $datum)))

  (define (context-syntax->compiled $context $syntax)
    (lets
      ($constants (context-constants $context))
      ($bindings (context-bindings $context))
      (syntax-case $syntax ()
        (($scheme $type $expr)
          (identifier-named? (syntax $scheme) scheme)
          (compiled-constant
            $constants
            (context-syntax->type
              $context
              (syntax $type))
            (constants-datum->value
              $constants
              (syntax->datum (syntax $expr)))))
        ($number
          (identifier-named? (syntax $number) number)
          (compiled-constant $constants (type-type) (number-type)))
        ($string
          (identifier-named? (syntax $string) string)
          (compiled-constant $constants (type-type) (string-type)))
        ($other
          (switch (syntax->datum (syntax $other))
            ((number? $number)
              (compiled-constant $constants (number-type) $number))
            ((string? $string)
              (compiled-constant $constants (string-type) $string))
            ((else $other)
              (syntax-error $other)))))))

  (define (args-compiler+syntax $args-compiler $syntax)
    (lets
      ($context (args-compiler-context $args-compiler))
      ($args (args-compiler-args $args-compiler))
      (syntax-case $syntax ()
        ($other
          (args-compiler+compiled
            (context-syntax->compiled $context $other))))))

  (define (args-compiler+compiled $args-compiler $compiled)
    (args-compiler
      (args-compiler-context $args-compiler)
      (push (args-compiler-args $args-compiler) $compiled)))

  (define (bindings-args->resolve $bindings $args)
    )

  (define (binding-args->resolve $binding $args)
    (lets
      ($type (binding-type $binding))
      ($symbol (binding-symbol $binding))
      (todo)))


  (define (compiled->value $compiled)
    (lets
      ($constants (compiled-constants $compiled))
      ($typed-expression (compiled-typed-expression $compiled))
      ($type (typed-type $typed-expression))
      ($expression (typed-value $typed-expression))
      (typed-value->type
        (typed $type
          (and $expression (constants-expression->value $constants $expression))))))

  (define (constants-expression->value $constants $expression)
    (lets
      (do
        (unless (= (expression-depth $expression) 0)
          (throw not-constant $expression)))
      (constants-datum->value
        $constants
        (expression-datum $expression))))

  (define (constants-datum->value $constants $datum)
    (evaluate
      (evaluator
        (environment `(micascheme))
        (map cons
          (map constant-symbol $constants)
          (map constant-value $constants)))
      $datum))

  (define (compiled-constant $constants $type $value)
    (lets
      ($symbol (generate-symbol))
      (compiled
        (push $constants (cons $symbol $value))
        (typed
          $type
          (expression $symbol 0)))))

  (define (typed-runtime->type $typed-runtime)
    (lets
      ($type (typed-type $typed-value))
      ($runtime (typed-value $typed-value))
      (switch $type
        ((value-type? $value-type)
          $value-type)
        ((boolean-type? _)
          (value-type (runtime-value $runtime)))
        ((number-type? _)
          (value-type (runtime-value $runtime)))
        ((string-type? _)
          (value-type (runtime-value $runtime)))
        ((struct-type? $struct-type)
          (struct-type
            (struct-type-name $struct-type)
            (map typed-value->value (struct-type-fields $struct-type))))
        ((else $other)
          (throw typed-value->value $other)))))

  (define (term-symbolize $term)
    (lets
      ($symbol (generate-symbol))
      (term
        (stack (cons $symbol (term->value $term)))
        $symbol)))

  (define (term->value $term)
    (evaluate
      (evaluator
        (environment `(micascheme))
        (term-bindings $term))
      (term-datum $term)))

  (define (term-ensure-symbolized $term)
    (cond
      ((symbol? (term-datum $term)) $term)
      (else (term-symbolize $term))))

  (define (thunk-ensure-optimized $thunk)
    (cond
      ((zero? (thunk-free-variable-count $thunk))
        (thunk (term-ensure-symbolized (thunk-term $thunk)) 0))
      (else $thunk)))
)
