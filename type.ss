(library (type)
  (export 
    type-is-static?
    matches? list-matches?
    type-selector type-named?
    type-selector-index)

  (import (micascheme) (term))

  ; ---------------------------------------------------------

  (define (type-is-static? $type)
    (switch $type
      ((native? _) #f)
      ((symbol? _) #t)
      ((boolean-type? _) #f)
      ((number-type? _) #f)
      ((string-type? _) #f)
      ((universe? _) #f)
      ((variable? _) #f)
      ((function? $function) (type-is-static? (function-body $function)))
      ((function-type? $function-type) (type-is-static? (function-type-result $function-type)))
      ((tuple-type? $tuple-type) (andmap type-is-static? (tuple-type-types $tuple-type)))
      ((else $obj) (throw type-is-static? $obj))))

  ; ---------------------------------------------------------

  (data (hole))

  ; TODO - Change argument order
  (define (matches? $lhs $rhs)
    (and (match `() $lhs $rhs) #t))

  (define (list-matches? $lhss $rhss)
    (andmap matches? $lhss $rhss))

  (define (list-match $env $lhs $rhs)
    (if (null? $lhs)
      (and (null? $rhs) $env)
      (and (not (null? $rhs))
        (and-lets ($env (match $env (car $lhs) (car $rhs)))
          (list-match $env (cdr $lhs) (cdr $rhs))))))
  
  (define (match $env $lhs $rhs)
    (switch $lhs
      ((native? $native)
        (native-match $env $native $rhs))
      ((symbol? $symbol)
        (symbol-match $env $symbol $rhs))
      ((boolean-type? $boolean-type)
        (boolean-type-match $env $boolean-type $rhs))
      ((number-type? $number-type)
        (number-type-match $env $number-type $rhs))
      ((string-type? $string-type)
        (string-type-match $env $string-type $rhs))
      ((universe? $universe)
        (universe-match $env $universe $rhs))
      ((variable? $variable) 
        (variable-match $env $variable $rhs))
      ((function? $function) 
        (function-match $env $function $rhs))
      ((function-type? $function-type) 
        (function-type-match $env $function-type $rhs))
      ((tuple-type? $tuple-type)
        (tuple-type-match $env $tuple-type $rhs))
      ((else $obj)
        (throw match $env $lhs $rhs))))

  (define (native-match $env $native $rhs)
    (and (native? $rhs) (equal? (native-term $native) (native-term $rhs))))

  (define (symbol-match $env $symbol $rhs)
    (and (symbol? $rhs) (symbol=? $symbol $rhs)))

  (define (boolean-type-match $env $boolean-type $rhs)
    (boolean-type? $rhs))

  (define (number-type-match $env $number-type $rhs)
    (number-type? $rhs))

  (define (string-type-match $env $string-type $rhs)
    (string-type? $rhs))

  (define (universe-match $env $universe $rhs)
    (and 
      (universe? $rhs)
      (= (universe-depth $universe) (universe-depth $rhs))))

  (define (variable-match $env $variable $rhs)
    (lets
      ($index (variable-index $variable))
      (if (variable? $rhs)
        (= (variable-index $variable) (variable-index $rhs))
        (switch (list-ref $env $index)
          ((hole? _) 
            (list-set $env $index $rhs))
          ((else $other) 
            (match $env $other $rhs))))))

  (define (function-match $env $function $rhs)
    (if (function? $rhs)
      (and 
        (= (function-arity $function) (function-arity $rhs))
        (match
          (iterate (partial cons (hole)) $env (function-arity $function))
          (function-body $function)
          (function-body $rhs)))
      (match 
        (iterate (partial cons (hole)) $env (function-arity $function))
        (function-body $function) 
        $rhs)))

  (define (function-type-match $env $function-type $rhs)
    (and
      (function-type? $rhs)
      (symbol=? (function-type-name $function-type) (function-type-name $rhs))
      (and-lets 
        ($env (list-match $env (function-type-params $function-type) (function-type-params $rhs)))
        (match $env (function-type-result $function-type) (function-type-result $rhs)))))

  (define (tuple-type-match $env $tuple-type $rhs)
    (and
      (tuple-type? $rhs)
      (symbol=?
        (tuple-type-name $tuple-type)
        (tuple-type-name $rhs))
      (list-match $env
        (tuple-type-types $tuple-type)
        (tuple-type-types $rhs))))

  ; ------------------------------------------------

  (define (type-selector $type)
    (switch $type
      ((symbol? $symbol) $symbol)
      ((boolean-type? _) `boolean)
      ((number-type? _) `number)
      ((string-type? _) `string)
      ((universe? _) `universe)
      ((function-type? _) `function)
      ((tuple-type? $tuple-type) (tuple-type-name $tuple-type))
      ((else $other) #f)))

  (define (type-named? $type $symbol)
    (and-lets ($selector (type-selector $type))
      (symbol=? $selector $symbol)))

  (define (type-selector-index $type $selector)
    (and (pair? $type)
      (and-lets 
        ($indexed
          (map-find-indexed 
            (lambda ($sub-type) 
              (and-lets ($sub-selector (type-selector $sub-type))
                (eq? $sub-selector $selector)))
            (cdr $type)))
        (indexed-index $indexed))))
)
