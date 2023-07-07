(library (type)
  (export 
    matches? list-matches?
    type-selector type-named?
    type-selector-index)

  (import (micascheme) (term))

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
        (bind-true ($env (match $env (car $lhs) (car $rhs)))
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
      ((abstraction? $abstraction) 
        (abstraction-match $env $abstraction $rhs))
      ((arrow? $arrow) 
        (arrow-match $env $arrow $rhs))
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
    (bind ($index (variable-index $variable))
      (if (variable? $rhs)
        (= (variable-index $variable) (variable-index $rhs))
        (switch (list-ref $env $index)
          ((hole? _) 
            (list-set $env $index $rhs))
          ((else $other) 
            (match $env $other $rhs))))))

  (define (abstraction-match $env $abstraction $rhs)
    (if (abstraction? $rhs)
      (and 
        (= (abstraction-arity $abstraction) (abstraction-arity $rhs))
        (match
          (iterate (partial cons (hole)) $env (abstraction-arity $abstraction))
          (abstraction-body $abstraction)
          (abstraction-body $rhs)))
      (match 
        (iterate (partial cons (hole)) $env (abstraction-arity $abstraction))
        (abstraction-body $abstraction) 
        $rhs)))

  (define (arrow-match $env $arrow $rhs)
    (and
      (arrow? $rhs)
      (symbol=? (arrow-name $arrow) (arrow-name $rhs))
      (bind-true 
        ($env (list-match $env (arrow-params $arrow) (arrow-params $rhs)))
        (match $env (arrow-result $arrow) (arrow-result $rhs)))))

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
      ((arrow? _) `arrow)
      ((tuple-type? $tuple-type) (tuple-type-name $tuple-type))
      ((else $other) #f)))

  (define (type-named? $type $symbol)
    (bind-true ($selector (type-selector $type))
      (symbol=? $selector $symbol)))

  (define (type-selector-index $type $selector)
    (and (pair? $type)
      (bind-true 
        ($indexed
          (map-find-indexed 
            (lambda ($sub-type) 
              (bind-true ($sub-selector (type-selector $sub-type))
                (eq? $sub-selector $selector)))
            (cdr $type)))
        (indexed-index $indexed))))
)
