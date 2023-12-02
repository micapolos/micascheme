(library (leo evaluator)
  (export
    evaluated evaluated? evaluated-value evaluated-type
    boolean! boolean-type boolean-type?
    number! number-type number-type?
    string! string-type string-type?

    tuple! tuple tuple? tuple-items
    evaluate)
  (import
    (except (micascheme) pair))

  (data (boolean-type))
  (data (number-type))
  (data (string-type))

  (data (tuple items))
  (data (tuple-get tuple type))
  (data (oneof items))
  (data (oneof-switch oneof cases))
  (data (variable type))
  (data (function params body))
  (data (application function args))
  (data (repeating function))
  (data (recursive function))
  (data (cast value type))

  (data (evaluated value type))

  (define boolean! (boolean-type))
  (define number! (number-type))
  (define string! (string-type))
  (define-syntax-rule (tuple! $item ...)
    (tuple (list $item ...)))

  (define (evaluate-list $values)
    (bind-if null-or-pair?
      (fold-while null-or-pair?
        (lambda ($stack $value)
          (switch (evaluate $value)
            ((evaluated? $evaluated) (push $stack $evaluated))
            ((else $other) $other)))
        (stack)
        $values)
      reverse))

  (define (evaluate $value)
    (switch $value
      ((evaluated? $evaluated)
        $evaluated)
      ((boolean? $boolean)
        (evaluated $boolean (boolean-type)))
      ((number? $number)
        (evaluated $number (number-type)))
      ((string? $string)
        (evaluated $string (string-type)))
      ((tuple? $tuple)
        (bind-if null-or-pair?
          (evaluate-list (tuple-items $tuple))
          (lambda ($evaluated-list)
            (evaluated
              (tuple (map evaluated-value $evaluated-list))
              (tuple (map evaluated-type $evaluated-list))))))
      ((else $other) $other)))
)
