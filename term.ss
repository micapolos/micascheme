(library (term)
  (export
    abstraction abstraction? abstraction-arity abstraction-body
    application application? application-fn application-args
    variable variable? variable-index
    term->datum)

  (import (micascheme))

  (data (variable index))
  (data (application fn args))
  (data (abstraction arity body))

  (define (term->datum $term)
    (depth-term->datum 0 $term))

  (define (depth-terms->datum $depth $terms)
    (map (partial depth-term->datum $depth) $terms))

  (define (depth-term->datum $depth $term)
    (switch $term
      ((variable? $variable) (depth-variable->datum $depth $variable))
      ((application? $application) (depth-application->datum $depth $application))
      ((abstraction? $abstraction) (depth-abstraction->datum $depth $abstraction))
      ((else $datum) $datum)))

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

  (define (depth->datum $depth)
    (string->symbol 
      (string-append "v" 
        (number->string $depth))))

  (define (depth-size->datums $depth $size)
    (map depth->datum 
      (map (partial + $depth)
        (indices $size))))
)