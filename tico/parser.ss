(library (tico parser)
  (export
    compiled compiled? compiled-type compiled-combo-opt
    combo combo? combo-constant-opt combo-expression
    constant constant? constant-value

    syntax->compiled datum->compiled)
  (import
    (micascheme)
    (tico expression)
    (tico value)
    (tico type))

  (data (context))
  (data (constant value))
  (data (combo constant-opt expression))
  (data (compiled type combo-opt))

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
      (($scheme $type $value)
        (identifier-named? (syntax $scheme) scheme)
        (compiled
          (context-syntax->type $context (syntax $type))
          (combo #f (syntax->datum (syntax $value)))))
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
            (compiled-literal (boolean-type) $boolean))
          ((number? $number)
            (compiled-literal (number-type) $number))
          ((string? $string)
            (compiled-literal (string-type) $string))
          ((else _) (syntax-error $syntax))))))

  (define (compiled-struct $name $fields)
    (lets
      ($types (map compiled-type $fields))
      ($combos (filter-opts (map compiled-combo-opt $fields)))
      ($constant-opts (map combo-constant-opt $combos))
      ($expressions (map combo-expression $combos))
      (compiled
        (struct-type $name $types)
        (and
          (not (null? $combos))
          (combo
            (and
              (for-all identity $constant-opts)
              (constant (tuple-value (map constant-value $constant-opts))))
            (tuple-expression $expressions))))))

  (define (compiled-literal $type $literal)
    (compiled (value-type $literal) #f))

  (define (context-syntax->type $context $syntax)
    (compiled-constant-type
      (context-syntax->compiled $context $syntax)))

  (define (compiled-constant-type $compiled)
    (type-constant-opt->type
      (compiled-type $compiled)
      (opt-lift combo-constant-opt (compiled-combo-opt $compiled))))

  (define (type-constant-opt->type $type $constant-opt)
    (switch $type
      ((value-type? $value-type)
        (value-type-value $value-type))
      ((else $other) (todo))))
)
