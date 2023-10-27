(library (tico parser)
  (export
    compiled compiled? compiled-type compiled-packet-opt
    packet packet? packet-expression packet-constant-opt
    expression expression? expression-datum expression-depth
    constant constant? constant-value
    value-compiled

    syntax->compiled datum->compiled)
  (import
    (micascheme)
    (rename (tico expression) (tuple-expression tuple-datum))
    (tico value)
    (tico type)
    (evaluator))

  (data (context))
  (data (compiled type packet-opt))
  (data (packet expression constant-opt))
  (data (expression datum depth))
  (data (constant value))

  (define null-context (context))

  (define (context-push-types $context $types)
    $context)

  (define (value-compiled $value)
    (compiled (value-type $value) #f))

  (define (datum-packet $datum)
    (packet
      (datum-expression $datum)
      (datum-constant $datum)))

  (define (datum-expression $datum)
    (expression $datum 0))

  (define (datum-constant $datum)
    (constant
      (evaluate
        (evaluator (environment `(micascheme)) (stack))
        $datum)))

  (define (datum->compiled $datum)
    (syntax->compiled (datum->syntax #`+ $datum)))

  (define (syntax->compiled $syntax)
    (context-syntax->compiled null-context $syntax))

  (define (context-syntax->compiled $context $syntax)
    (syntax-case $syntax ()
      ($boolean
        (identifier-named? (syntax $boolean) boolean)
        (value-compiled (boolean-type)))
      ($number
        (identifier-named? (syntax $number) number)
        (value-compiled (number-type)))
      ($string
        (identifier-named? (syntax $string) string)
        (value-compiled (string-type)))
      (($scheme $type $expr)
        (identifier-named? (syntax $scheme) scheme)
        (compiled
          (context-syntax->type $context (syntax $type))
          (datum-packet (syntax->datum (syntax $expr)))))
      (($lambda ($params ...) $body)
        (identifier-named? (syntax $lambda) lambda)
        (lets
          ($params (map (partial context-syntax->type $context) (syntax->list (syntax ($params ...)))))
          ($context (context-push-types $context $params))
          ($compiled-body (context-syntax->compiled $context (syntax $body)))
          (compiled
            (lambda-type $params (compiled-type $compiled-body))
            (packet
              (expression `todo 0)
              (constant `todo)))))
      (($type $expr)
        (identifier-named? (syntax $type) type)
        (value-compiled
          (compiled-type (context-syntax->compiled $context (syntax $expr)))))
      (($struct $name $fields ...)
        (identifier-named? (syntax $struct) struct)
        (compiled-struct
          (ensure symbol? (syntax->datum (syntax $name)))
          (map
            (partial context-syntax->compiled $context)
            (syntax->list (syntax ($fields ...))))))
      (($symbol $args ...)
        (identifier? (syntax $symbol))
        (context-syntax->compiled $context (syntax (struct $symbol $args ...))))
      ($other
        (switch (syntax->datum (syntax $other))
          ((boolean? $boolean)
            (value-compiled $boolean))
          ((number? $number)
            (value-compiled $number))
          ((string? $string)
            (value-compiled $string))
          ((else _)
            (syntax-error $syntax))))))

  (define (compiled-struct $name $fields)
    (lets
      ($types (map compiled-type $fields))
      ($packets (filter-opts (map compiled-packet-opt $fields)))
      (compiled
        (struct-type $name $types)
        (and
          (not (null? $packets))
          (tuple-packet $packets)))))

  (define (tuple-packet $packets)
    (packet
      (tuple-expression (map packet-expression $packets))
      (tuple-constant-opt (map packet-constant-opt $packets))))

  (define (tuple-expression $expressions)
    (expression
      (tuple-datum (map expression-datum $expressions))
      (apply max (map expression-depth $expressions))))

  (define (tuple-constant-opt $constant-opts)
    (and
      (for-all identity $constant-opts)
      (constant (tuple-value (map constant-value $constant-opts)))))

  (define (context-syntax->type $context $syntax)
    (switch (compiled-type (context-syntax->compiled $context $syntax))
      ((value-type? $value-type)
        (value-type-value $value-type))
      ((else $other)
        (throw not-type $other))))
)
