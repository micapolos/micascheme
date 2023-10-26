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
      (($symbol $args ...)
        (identifier? (syntax $symbol))
        (lets
          ($compileds
            (map
              (partial context-syntax->compiled $context)
              (syntax->list (syntax ($args ...)))))
          ($types (map compiled-type $compileds))
          ($combos (filter-opts (map compiled-combo-opt $compileds)))
          ($constant-opts (map combo-constant-opt $combos))
          ($expressions (map combo-expression $combos))
          (compiled
            (struct-type (syntax->datum (syntax $symbol)) $types)
            (and
              (not (null? $combos))
              (combo
                (and
                  (for-all identity $constant-opts)
                  (constant (tuple-value (map constant-value $constant-opts))))
                (tuple-expression $expressions))))))
      ($other
        (switch (syntax->datum (syntax $other))
          ((boolean? $boolean)
            (compiled-literal (boolean-type) $boolean))
          ((number? $number)
            (compiled-literal (number-type) $number))
          ((string? $string)
            (compiled-literal (string-type) $string))
          ((else _) (syntax-error $syntax))))))

  (define (compiled-literal $type $literal)
    (compiled $type (combo (constant $literal) $literal)))
)
