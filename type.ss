(library (type)
  (export 
    variable application abstraction
    match arrow matches?
    term->datum
    typed parse evaluate)

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
      ,(term-depth->datum (application-rhs $application) $depth)))

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

  ; ----------------------------------------------------------------

  (data (typed value type))

  (define (evaluate $stx)
    (let 
      (($typed 
        (parse 
          (list 
            (cons `string-length (arrow `string `number))
            (cons `number->string (arrow `number `string)))
          $stx)))
      (typed 
        (eval 
          `(let 
            ((v1 string-length)
             (v0 number->string))
            ,(typed-value $typed)) 
          (environment `(micascheme) `(type)))
        (typed-type $typed))))

  (define (parse-as $env $stx $as-type)
    (let* (($typed (parse $env $stx))
           ($type (typed-type $typed)))
      (if (matches? $as-type $typed)
        (typed (typed-value $typed) $as-type)
        (syntax-error $stx
          (format "should be ~s, is ~s:" $as-type $type)))))

  (define (parse $env $stx)
    (syntax-case $stx (lambda)
      ((lambda (type var) body)
        (syntax-error $stx "Jeszcze tego nie umiem"))
      ((lhs rhs)
        (let* (($typed-lhs (parse $env #`lhs))
               ($typed-rhs (parse $env #`rhs))
               ($lhs-type (typed-type $typed-lhs))
               ($rhs-type (typed-type $typed-rhs)))
          (unless (arrow? $lhs-type)
            (syntax-error #`lhs (format "should be procedure, is ~s:" $lhs-type)))
          (unless (matches? (arrow-lhs $lhs-type) $rhs-type)
            (syntax-error #`rhs (format "should be ~s, is ~s:" $rhs-type (arrow-lhs $lhs-type))))
          (typed 
            `(,(typed-value $typed-lhs) ,(typed-value $typed-rhs))
            (arrow-rhs $lhs-type))))
      (_
        (switch (syntax->datum $stx)
          ((boolean? $boolean) 
            (typed $boolean `boolean))
          ((number? $number)
            (typed $number `number))
          ((string? $string) 
            (typed $string `string))
          ((symbol? $symbol)
            (bind ($index-type (associ $env 0 $symbol))
              (if $index-type
                (typed 
                  (string->symbol 
                    (string-append "v" 
                      (number->string 
                        (- (length $env) (car $index-type) 1))))
                  (cdr $index-type))
                (syntax-error $stx "unbound:"))))))))
)
