(library (tico parser)
  (export
    compiled compiled? compiled-type compiled-expr-opt
    expr expr? expr-datum

    syntax->compiled datum->compiled)
  (import
    (micascheme)
    (tico expression)
    (tico value)
    (tico type))

  (data (context))
  (data (compiled type expr-opt))
  (data (expr datum))

  (define null-context (context))

  (define (datum->compiled $datum)
    (syntax->compiled (datum->syntax #`+ $datum)))

  (define (syntax->compiled $syntax)
    (context-syntax->compiled null-context $syntax))

  (define (context-syntax->compiled $context $syntax)
    (syntax-case $syntax ()
      ($boolean
        (identifier-named? (syntax $boolean) boolean)
        (compiled (value-type (boolean-type)) #f))
      ($number
        (identifier-named? (syntax $number) number)
        (compiled (value-type (number-type)) #f))
      ($string
        (identifier-named? (syntax $string) string)
        (compiled (value-type (string-type)) #f))
      (($scheme $type $expr)
        (identifier-named? (syntax $scheme) scheme)
        (compiled
          (context-syntax->type $context (syntax $type))
          (expr (syntax->datum (syntax $expr)))))
      (($type $expr)
        (identifier-named? (syntax $type) type)
        (compiled
          (value-type (compiled-type (context-syntax->compiled $context (syntax $expr))))
          #f))
      (($symbol $args ...)
        (identifier? (syntax $symbol))
        (compiled-struct
          (syntax->datum (syntax $symbol))
          (map
            (partial context-syntax->compiled $context)
            (syntax->list (syntax ($args ...))))))
      ($other
        (switch (syntax->datum (syntax $other))
          ((boolean? $boolean)
            (compiled (value-type $boolean) #f))
          ((number? $number)
            (compiled (value-type $number) #f))
          ((string? $string)
            (compiled (value-type $string) #f))
          ((else _) (syntax-error $syntax))))))

  (define (compiled-struct $name $fields)
    (lets
      ($types (map compiled-type $fields))
      ($exprs (filter-opts (map compiled-expr-opt $fields)))
      (compiled
        (struct-type $name $types)
        (and
          (not (null? $exprs))
          (expr (tuple-expression (map expr-datum $exprs)))))))

  (define (context-syntax->type $context $syntax)
    (switch (compiled-type (context-syntax->compiled $context $syntax))
      ((value-type? $value-type)
        (value-type-value $value-type))
      ((else $other)
        (throw not-type $other))))
)
