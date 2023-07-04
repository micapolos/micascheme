(library (type)
  (export 
    variable application abstraction
    match arrow matches?
    term->datum)

  (import (micascheme))

  (data (variable index))
  (data (application lhs rhs))
  (data (abstraction body))

  ; --------------------------------------------------------

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
      ,@(term-depth->datum (application-rhs $application) $depth)))

  (define (abstraction-depth->datum $abstraction $depth)
    `(lambda (,(depth->datum $depth))
      ,(term-depth->datum 
        (abstraction-body $abstraction) 
        (+ $depth 1))))

  (define (depth->datum $depth)
    (string->symbol 
      (string-append "v" 
        (number->string $depth))))

  ; ---------------------------------------------------------

  (data (arrow lhs rhs))
  (data (hole))

  (define (matches? $lhs $rhs)
    (and (match `() $lhs $rhs) #t))
  
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
      (match 
        (cons (hole) $env)
        (abstraction-body $abstraction)
        (abstraction-body $rhs))
      (match 
        (cons (hole) $env) 
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