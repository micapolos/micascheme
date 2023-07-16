(library (type)
  (export 
    type-static? type-dynamic?
    types-indexed
    matches? list-matches?
    type-selector type-named?
    type-selector-index
    choice-type-index-of)

  (import (micascheme) (term))

  ; ---------------------------------------------------------

  (define (type-static? $type)
    (switch $type
      ((native? _) #f)
      ((symbol? _) #t)
      ((boolean-type? _) #f)
      ((number-type? _) #f)
      ((string-type? _) #f)
      ((universe? _) #f)
      ((variable? _) #f)
      ((forall? $forall) (type-static? (forall-body $forall)))
      ((function? $function) (type-static? (function-body $function)))
      ((function-type? $function-type) (type-static? (function-type-result $function-type)))
      ((tuple-type? $tuple-type) (andmap type-static? (tuple-type-types $tuple-type)))
      ((else $obj) (throw type-static? $obj))))

  (define (type-dynamic? $type)
    (not (type-static? $type)))
  
  ; ---------------------------------------------------------

  (define (types-indexed $types)
    (filter 
      (lambda ($indexed) 
        (and (type-dynamic? (indexed-value $indexed)) $indexed))
      (list-indexed $types)))

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
      ((function-type? $function-type) 
        (function-type-match $env $function-type $rhs))
      ((forall? $forall)
        (forall-match $env $forall $rhs))
      ((tuple-type? $tuple-type)
        (tuple-type-match $env $tuple-type $rhs))
      ((else $obj)
        (throw match $env $lhs $rhs))))

  (define (native-match $env $native $rhs)
    (and 
      (native? $rhs) 
      (equal? (native-value $native) (native-value $rhs))
      $env))

  (define (symbol-match $env $symbol $rhs)
    (and 
      (symbol? $rhs) 
      (symbol=? $symbol $rhs) 
      $env))

  (define (boolean-type-match $env $boolean-type $rhs)
    (and (boolean-type? $rhs) $env))

  (define (number-type-match $env $number-type $rhs)
    (and (number-type? $rhs) $env))

  (define (string-type-match $env $string-type $rhs)
    (and (string-type? $rhs) $env))

  (define (universe-match $env $universe $rhs)
    (and 
      (universe? $rhs)
      (= (universe-depth $universe) (universe-depth $rhs))
      $env))

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

  (define (function-type-match $env $function-type $rhs)
    (and
      (function-type? $rhs)
      (symbol=? (function-type-name $function-type) (function-type-name $rhs))
      (and-lets 
        ($env (list-match $env (function-type-params $function-type) (function-type-params $rhs)))
        (match $env (function-type-result $function-type) (function-type-result $rhs)))))

  (define (forall-match $env $forall $rhs)
    (lets
      ($arity (forall-arity $forall))
      ($body (forall-body $forall))
      ($env (iterate (partial cons (hole)) $env $arity))
      (if (forall? $rhs)
        (and 
          (= $arity (forall-arity $rhs))
          (match $env $body (forall-body $rhs)))
        (match $env $body $rhs))))

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

  ; -------------------------------------------------

  (define (choice-type-index-of $choice-type $type)
    (and-lets 
      ($indexed (map-find-indexed (partial obj=? $type) (choice-type-types $choice-type)))
      (indexed-index $indexed)))
)
