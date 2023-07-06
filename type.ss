(library (type)
  (export 
    matches?
    type-selector
    type-selector-index)

  (import (micascheme) (term))

  ; ---------------------------------------------------------

  (data (hole))

  ; TODO - Change argument order
  (define (matches? $lhs $rhs)
    (and (match `() $lhs $rhs) #t))

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
      ((any-boolean? $any-boolean)
        (any-boolean-match $env $any-boolean $rhs))
      ((any-number? $any-number)
        (any-number-match $env $any-number $rhs))
      ((any-string? $any-string)
        (any-string-match $env $any-string $rhs))
      ((any-type? $any-type)
        (any-type-match $env $any-type $rhs))
      ((variable? $variable) 
        (variable-match $env $variable $rhs))
      ((abstraction? $abstraction) 
        (abstraction-match $env $abstraction $rhs))
      ((arrow? $arrow) 
        (arrow-match $env $arrow $rhs))
      ((struct? $struct)
        (struct-match $env $struct $rhs))
      ((else $obj)
        (throw match $env $lhs $rhs))))

  (define (native-match $env $native $rhs)
    (and (native? $rhs) (equal? (native-term $native) (native-term $rhs))))

  (define (symbol-match $env $symbol $rhs)
    (and (symbol? $rhs) (symbol=? $symbol $rhs)))

  (define (any-boolean-match $env $any-boolean $rhs)
    (any-boolean? $rhs))

  (define (any-number-match $env $any-number $rhs)
    (any-number? $rhs))

  (define (any-string-match $env $any-string $rhs)
    (any-string? $rhs))

  (define (any-type-match $env $any-type $rhs)
    (any-type? $rhs))

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
      (bind-true 
        ($env (match $env (arrow-lhs $arrow) (arrow-lhs $rhs)))
        (match $env (arrow-rhs $arrow) (arrow-rhs $rhs)))))

  (define (struct-match $env $struct $rhs)
    (and
      (struct? $rhs)
      (symbol=?
        (struct-name $struct)
        (struct-name $rhs))
      (list-match $env
        (struct-types $struct)
        (struct-types $rhs))))

  ; ------------------------------------------------

  (define (type-selector $type)
    (switch $type
      ((symbol? $symbol) $symbol)
      ((any-boolean? _) `boolean)
      ((any-number? _) `number)
      ((any-string? _) `string)
      ((any-type? _) `type)
      ((pair? $pair) (bind ($car (car $pair)) (and (symbol? $car) $car)))
      ((else $other) #f)))

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
