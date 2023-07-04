(library (type)
  (export 
    variable application abstraction
    term->datum)

  (import (micascheme))

  (data (variable index))
  (data (application lhs rhss))
  (data (abstraction arity body))

  (define (term->datum $term)
    (term-depth->datum $term 0))

  (define (term-depth->datum $term $depth)
    (switch $term
      ((variable? $variable) (variable-depth->datum $variable $depth))
      ((application? $application) (application-depth->datum $application $depth))
      ((abstraction? $abstraction) (abstraction-depth->datum $abstraction $depth))
      ((else $datum) $datum)))

  (define (variable-depth->datum $variable $depth)
    (let (($index (- $depth (variable-index $variable) 1)))
      (if (< $index 0) 
        (throw variable-depth->datum $variable $depth)
        (depth->datum $index))))

  (define (application-depth->datum $application $depth)
    `(
      ,(term-depth->datum (application-lhs $application) $depth)
      ,@(term-depth->datum (application-rhss $application) $depth)))

  (define (abstraction-depth->datum $abstraction $depth)
    `(lambda (,(depth->datum $depth))
      ,(term-depth->datum 
        (abstraction-body $abstraction) 
        (+ $depth (abstraction-arity $abstraction)))))

  (define (depth->datum $depth)
    (string->symbol 
      (string-append "v" 
        (number->string $depth))))
)