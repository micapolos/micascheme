(library (term)
  (export
    abstraction abstraction? abstraction-arity abstraction-body
    application application? application-fn application-args
    variable variable? variable-index

    any-boolean any-boolean?
    any-number any-number?
    any-string any-string?
    any-type any-type?
    
    arrow arrow? arrow-lhs arrow-rhs

    term->datum eval-term

    application!)

  (import (micascheme))

  (data (variable index))
  (data (application fn args))
  (data (abstraction arity body))

  (data (any-boolean))
  (data (any-number))
  (data (any-string))
  (data (any-type))

  (data (arrow lhs rhs))

  (define (term->datum $term)
    (depth-term->datum 0 $term))

  (define (depth-terms->datum $depth $terms)
    (map (partial depth-term->datum $depth) $terms))

  (define (depth-term->datum $depth $term)
    (switch $term
      ((symbol? $symbol) `(quote ,$symbol))
      ((boolean? $string) $string)
      ((number? $number) $number)
      ((string? $string) $string)
      ((any-boolean? _) `(any-boolean))
      ((any-number? _) `(any-number))
      ((any-string? _) `(any-string))
      ((any-type? _) `(any-type))
      ((variable? $variable) (depth-variable->datum $depth $variable))
      ((application? $application) (depth-application->datum $depth $application))
      ((abstraction? $abstraction) (depth-abstraction->datum $depth $abstraction))
      ((arrow? $arrow) (depth-arrow->datum $depth $arrow))
      ((else _) (throw depth-term->datum $depth $term))))

  (define (depth-variable->datum $depth $variable)
    (let (($index (- $depth (variable-index $variable) 1)))
      (if (< $index 0) 
        (throw depth-variable->datum $depth $variable)
        (depth->datum $index))))

  (define (depth-application->datum $depth $application)
    `(
      ,(depth-term->datum $depth (application-fn $application))
      ,@(depth-terms->datum $depth (application-args $application))))

  (define (depth-abstraction->datum $depth $abstraction)
    (bind ($arity (abstraction-arity $abstraction))
      `(lambda (,@(depth-size->datums $depth $arity))
        ,(depth-term->datum 
          (+ $depth $arity)
          (abstraction-body $abstraction)))))

  (define (depth-arrow->datum $depth $arrow)
    `(arrow
      ,(depth-term->datum $depth (arrow-lhs $arrow))
      ,(depth-term->datum $depth (arrow-rhs $arrow))))

  (define (depth->datum $depth)
    (string->symbol 
      (string-append "v" 
        (number->string $depth))))

  (define (depth-size->datums $depth $size)
    (map depth->datum 
      (map (partial + $depth)
        (indices $size))))

  ; -----------------------------------------------

  (define-syntax application!
    (lambda (stx)
      (syntax-case stx ()
        ((_ fn args ...)
          #`(application fn (list args ...))))))

  ; -----------------------------------------------

  (define (eval-term $term $env)
    (eval (term->datum $term) $env))
)