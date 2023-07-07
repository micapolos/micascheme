(library (term)
  (export
    native native? native-term

    function function? function-arity function-body
    function-type function-type? function-type-name function-type-params function-type-result

    application application? application-fn application-args
    variable variable? variable-index

    boolean-type boolean-type?
    number-type number-type?
    string-type string-type?

    tuple tuple? tuple-terms
    tuple-ref tuple-ref? tuple-ref-size tuple-ref-term tuple-ref-index
    tuple-type tuple-type? tuple-type-name tuple-type-types

    universe universe? universe-depth
    
    term->datum eval-term

    application! tuple! function-type! tuple-type!
    boolean! number! string! type!)

  (import (micascheme))

  (data (native term))

  (data (variable index))
  (data (application fn args))
  (data (function arity body))

  (data (boolean-type))
  (data (number-type))
  (data (string-type))
  (data (universe depth))

  (data (function-type name params result))

  (data (tuple terms))
  (data (tuple-ref size term index))
  (data (tuple-type name types))

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
      ((universe? $universe) `(universe ,(universe-depth $universe)))
      ((variable? $variable) (depth-variable->datum $depth $variable))
      ((application? $application) (depth-application->datum $depth $application))
      ((function? $function) (depth-function->datum $depth $function))
      ((function-type? $function-type) (depth-function-type->datum $depth $function-type))
      ((tuple? $tuple) (depth-tuple->datum $depth $tuple))
      ((tuple-ref? $tuple-ref) (depth-tuple-ref->datum $depth $tuple-ref))
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

  (define (depth-function->datum $depth $function)
    (bind ($arity (function-arity $function))
      `(lambda (,@(depth-size->datums $depth $arity))
        ,(depth-term->datum 
          (+ $depth $arity)
          (function-body $function)))))

  (define (depth-function-type->datum $depth $function-type)
    `(function-type
      (quote ,(function-type-name $function-type))
      (list ,@(depth-terms->datums $depth (function-type-params $function-type)))
      ,(depth-term->datum $depth (function-type-result $function-type))))

  (define (depth-tuple->datum $depth $tuple)
    (let* (($terms (tuple-terms $tuple))
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

  (define (depth-tuple-ref->datum $depth $tuple-ref)
    (let* (($size (tuple-ref-size $tuple-ref))
           ($term (tuple-ref-term $tuple-ref))
           ($datum (depth-term->datum $depth $term))
           ($index (tuple-ref-index $tuple-ref)))
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
        (_ #`(universe 0)))))

  (define-syntax application!
    (lambda (stx)
      (syntax-case stx ()
        ((_ fn arg ...)
          #`(application fn (list arg ...))))))

  (define-syntax function-type!
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name arg ...) result)
          #`(function-type (quote name) (list arg ...) result)))))

  (define-syntax tuple!
    (lambda (stx)
      (syntax-case stx ()
        ((_ arg ...)
          #`(tuple (list arg ...))))))

  (define-syntax tuple-type!
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name arg ...))
          #`(tuple-type (quote name) (list arg ...))))))

  ; -----------------------------------------------

  (define (eval-term $term $env)
    (eval (term->datum $term) $env))
)
