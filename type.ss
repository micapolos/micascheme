(library (type)
  (export 
    variable application abstraction
    match arrow matches?
    term->datum
    typed parse evaluate
    any-boolean any-string any-number)

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

  ; ----------------------------------------------------------------

  (define (v $index)
    (string->symbol
      (string-append "v"
        (number->string $index))))

  (define (parse-list $env $stxs)
    (map (partial parse $env) $stxs))

  (define (parse-as $env $stx $as-type)
    (let* (($typed (parse $env $stx))
           ($type (typed-type $typed)))
      (if (matches? $as-type $typed)
        (typed (typed-value $typed) $as-type)
        (syntax-error $stx
          (format "should be ~s, is ~s:" $as-type $type)))))

  (define (parse-proc $env $stx)
    (let* (($typed (parse $env $stx))
           ($type (typed-type $typed)))
      (and (arrow? $type)
        (syntax-error $stx
          (format "should be procedure, is ~s:" $type)))))

  (define (parse $env $stx)
    (syntax-case $stx (lambda)
      ((id arg ...) (identifier? #`id)
        (let* (($id (syntax->datum #`id))
               ($args (syntax->list #`(arg ...)))
               ($typed-args (parse-list $env $args))
               ($arg-types (map typed-type $typed-args))
               ($arg-values (map typed-value $typed-args))
               ($type `(,$id ,@$arg-types))
               ($indexed-result-type 
                (map-find-indexed 
                  (lambda ($env-type) 
                    (and 
                      (arrow? $env-type) 
                      (matches? (arrow-lhs $env-type) $type) 
                      (arrow-rhs $env-type)))
                  $env)))
          (if $indexed-result-type
            (let* (($result-type (indexed-value $indexed-result-type))
                   ($result-index (indexed-index $indexed-result-type))
                   ($var (v $result-index)))
              (typed `(,$var ,@$arg-values) $result-type))
            (typed
              `(,$id ,@$arg-values)
              `(,$id ,@$arg-types)))))
      (_
        (switch (syntax->datum $stx)
          ((boolean? $boolean) 
            (typed $boolean (any-boolean)))
          ((number? $number)
            (typed $number (any-number)))
          ((string? $string) 
            (typed $string (any-string)))
          ((symbol? $symbol)
            (typed $symbol $symbol))))))

  ; --------------------------------------------------------

  (define (evaluate $env $stx)
    (let* (($ids (map car $env))
           ($types (map cdr $env))
           ($typed (parse $types $stx))
           ($let-items (map-indexed (lambda ($index $id) `(,(v $index) ,$id)) $ids)))
      (typed
        (eval 
          `(let (,@$let-items) ,(typed-value $typed))
          (environment `(micascheme) `(type)))
        (typed-type $typed))))

)
