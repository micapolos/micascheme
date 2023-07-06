(library (term)
  (export
    native native? native-term

    abstraction abstraction? abstraction-arity abstraction-body
    application application? application-fn application-args
    variable variable? variable-index

    boolean-type boolean-type?
    number-type number-type?
    string-type string-type?
    tuple-type tuple-type? tuple-type-name tuple-type-types

    type-type type-type?
    
    arrow arrow? arrow-name arrow-params arrow-result

    make-tuple make-tuple? make-tuple-terms
    tuple-get tuple-get? tuple-get-size tuple-get-term tuple-get-index

    term->datum eval-term

    application! tuple! arrow! tuple-type!
    boolean! number! string! type!)

  (import (micascheme))

  (data (native term))

  (data (variable index))
  (data (application fn args))
  (data (abstraction arity body))

  (data (boolean-type))
  (data (number-type))
  (data (string-type))
  (data (tuple-type name types))
  (data (type-type))

  (data (arrow name params result))

  (data (make-tuple terms))
  (data (tuple-get size term index))

  (define (term->datum $term)
    (depth-term->datum 0 $term))

  (define (depth-terms->datums $depth $terms)
    (map (partial depth-term->datum $depth) $terms))

  (define (depth-term->datum $depth $term)
    (switch $term
      ((native? $native) (depth-native->datum $depth $native))
      ((symbol? $symbol) `(quote ,$symbol))
      ((boolean? $string) $string)
      ((number? $number) $number)
      ((string? $string) $string)
      ((boolean-type? _) `(boolean-type))
      ((number-type? _) `(number-type))
      ((string-type? _) `(string-type))
      ((tuple-type? $tuple-type) (depth-tuple-type->datum $depth $tuple-type))
      ((type-type? _) `(type-type))
      ((variable? $variable) (depth-variable->datum $depth $variable))
      ((application? $application) (depth-application->datum $depth $application))
      ((abstraction? $abstraction) (depth-abstraction->datum $depth $abstraction))
      ((arrow? $arrow) (depth-arrow->datum $depth $arrow))
      ((make-tuple? $make-tuple) (depth-make-tuple->datum $depth $make-tuple))
      ((tuple-get? $tuple-get) (depth-tuple-get->datum $depth $tuple-get))
      ((else _) (throw depth-term->datum $depth $term))))

  (define (depth-native->datum $depth $native)
    (native-term $native))

  (define (depth-variable->datum $depth $variable)
    (let (($index (- $depth (variable-index $variable) 1)))
      (if (< $index 0) 
        (throw depth-variable->datum $depth $variable)
        (depth->datum $index))))

  (define (depth-application->datum $depth $application)
    `(
      ,(depth-term->datum $depth (application-fn $application))
      ,@(depth-terms->datums $depth (application-args $application))))

  (define (depth-abstraction->datum $depth $abstraction)
    (bind ($arity (abstraction-arity $abstraction))
      `(lambda (,@(depth-size->datums $depth $arity))
        ,(depth-term->datum 
          (+ $depth $arity)
          (abstraction-body $abstraction)))))

  (define (depth-arrow->datum $depth $arrow)
    `(arrow
      (quote ,(arrow-name $arrow))
      (list ,@(depth-terms->datums $depth (arrow-params $arrow)))
      ,(depth-term->datum $depth (arrow-result $arrow))))

  (define (depth-make-tuple->datum $depth $make-tuple)
    (let* (($terms (make-tuple-terms $make-tuple))
           ($size (length $terms))
           ($datums (depth-terms->datums $depth $terms)))
      (case $size
        ((0) #f)
        ((1) (car $datums))
        ((2) `(cons ,(car $datums) ,(cadr $datums)))
        (else `(vector ,@$datums)))))

  (define (depth-tuple-type->datum $depth $tuple-type)
    `(tuple-type
      (quote ,(tuple-type-name $tuple-type))
      (list
        ,@(depth-terms->datums
          $depth
          (tuple-type-types $tuple-type)))))

  (define (depth-tuple-get->datum $depth $tuple-get)
    (let* (($size (tuple-get-size $tuple-get))
           ($term (tuple-get-term $tuple-get))
           ($datum (depth-term->datum $depth $term))
           ($index (tuple-get-index $tuple-get)))
      (case $size
        ((0) #f)
        ((1) $datum)
        ((2) `(,(if (= $index 0) `car `cdr) ,$datum))
        (else `(vector-ref ,$datum ,$index)))))

  (define (depth->datum $depth)
    (string->symbol 
      (string-append "v" 
        (number->string $depth))))

  (define (depth-size->datums $depth $size)
    (map depth->datum 
      (map (partial + $depth)
        (indices $size))))

  ; -----------------------------------------------

  (define-syntax boolean!
    (lambda (stx)
      (syntax-case stx ()
        (_ #`(boolean-type)))))

  (define-syntax number!
    (lambda (stx)
      (syntax-case stx ()
        (_ #`(number-type)))))

  (define-syntax string!
    (lambda (stx)
      (syntax-case stx ()
        (_ #`(string-type)))))

  (define-syntax type!
    (lambda (stx)
      (syntax-case stx ()
        (_ #`(type-type)))))

  (define-syntax application!
    (lambda (stx)
      (syntax-case stx ()
        ((_ fn arg ...)
          #`(application fn (list arg ...))))))

  (define-syntax arrow!
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name arg ...) result)
          #`(arrow (quote name) (list arg ...) result)))))

  (define-syntax tuple!
    (lambda (stx)
      (syntax-case stx ()
        ((_ arg ...)
          #`(make-tuple (list arg ...))))))

  (define-syntax tuple-type!
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name arg ...))
          #`(tuple-type (quote name) (list arg ...))))))

  ; -----------------------------------------------

  (define (eval-term $term $env)
    (eval (term->datum $term) $env))
)
