(library (type)
  (export 
    any-boolean any-boolean?
    any-number any-number?
    any-string any-string?
    any-type any-type?
    
    arrow arrow? arrow-lhs arrow-rhs

    matches?)

  (import (micascheme) (term))

  ; ---------------------------------------------------------

  (data (any-boolean))
  (data (any-number))
  (data (any-string))
  (data (any-type))

  (data (arrow lhs rhs))

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
      ((variable? $variable) 
        (variable-match $env $variable $rhs))
      ((abstraction? $abstraction) 
        (abstraction-match $env $abstraction $rhs))
      ((arrow? $arrow) 
        (arrow-match $env $arrow $rhs))
      ((else $obj) 
        (obj-match $env $obj $rhs))))

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

  (define (obj-match $env $obj $rhs)
    (and (obj=? $obj $rhs) $env))

)
