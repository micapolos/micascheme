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

    tuple tuple? tuple-name tuple-items
    tuple-ref tuple-ref? tuple-ref-tuple tuple-ref-index
    tuple-type tuple-type? tuple-type-name tuple-type-types

    choice-type choice-type? choice-type-types
    choice-switch choice-switch? choice-switch-size choice-switch-term choice-switch-cases
    select select? select-size select-index select-term

    universe universe? universe-depth
    
    typed typed? typed-value typed-type
    typed-tuple typed-tuple!
    typed-function typed-function!

    term->datum eval-term

    application! tuple! function-type! tuple-type! choice-type!
    boolean! number! string! type! typed!)

  (import (micascheme))

  (data (typed value type))

  (data (native term))

  (data (variable index))
  (data (application fn args))
  (data (function arity body))

  (data (boolean-type))
  (data (number-type))
  (data (string-type))
  (data (universe depth))

  (data (function-type name params result))

  (data (tuple name items))
  (data (tuple-ref tuple index))
  (data (tuple-type name types))

  (data (choice-type types))
  (data (choice-switch size term cases))
  (data (select size index term))

  ; --------------------------------------------------

  (define (typed-tuple $name $items)
    (typed
      (tuple $name $items)
      (tuple-type $name (map typed-type $items))))

  (define (typed-function $name $param-types $body)
    (typed
      (function (length $param-types) (typed-value $body))
      (function-type $name $param-types (typed-type $body))))

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
      ((universe? $universe) `(universe ,(universe-depth $universe)))
      ((variable? $variable) (depth-variable->datum $depth $variable))
      ((application? $application) (depth-application->datum $depth $application))
      ((function? $function) (depth-function->datum $depth $function))
      ((function-type? $function-type) (depth-function-type->datum $depth $function-type))
      ((tuple? $tuple) (depth-tuple->datum $depth $tuple))
      ((tuple-ref? $tuple-ref) (depth-tuple-ref->datum $depth $tuple-ref))
      ((tuple-type? $tuple-type) (depth-tuple-type->datum $depth $tuple-type))
      ((select? $select) (depth-select->datum $depth $select))
      ((choice-switch? $choice-switch) (depth-choice-switch->datum $depth $choice-switch))
      ((choice-type? $choice-type) (depth-choice-type->datum $depth $choice-type))
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
    (lets
      ($arity (function-arity $function))
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
    (lets
      ($items (tuple-items $tuple))
      ($terms (map typed-value $items))
      ($size (length $terms))
      ($datums (depth-terms->datums $depth $terms))
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
    (lets
      ($tuple (tuple-ref-tuple $tuple-ref))
      ($type (typed-type $tuple))
      ($term (typed-value $tuple))
      ($size (length (tuple-type-types $type)))
      ($datum (depth-term->datum $depth $term))
      ($index (tuple-ref-index $tuple-ref))
      (case $size
        ((0) #f)
        ((1) $datum)
        ((2) `(,(if (= $index 0) `car `cdr) ,$datum))
        (else `(vector-ref ,$datum ,$index)))))

  (define (depth-select->datum $depth $select)
    (lets
      ($size (select-size $select))
      ($index (select-index $select))
      ($term (select-term $select))
      ($datum (depth-term->datum $depth $term))
      (case $size
        ((1) $datum)
        ((2) `(cons ,(= $index 1) ,$datum))
        (else `(cons ,$index ,$datum)))))

  (define (depth-choice-switch->datum $depth $choice-switch)
    (lets
      ($size (choice-switch-size $choice-switch))
      ($term (choice-switch-term $choice-switch))
      ($cases (choice-switch-cases $choice-switch))
      ($datum (depth-term->datum $depth $term))
      ($var (depth->datum $depth))
      `(let ((,$var ,$datum))
        ,(case $size
          ((1) (depth-term->datum (+ $depth 1) (car $cases)))
          (else
            (lets 
              ($value-var (depth->datum (+ $depth 1)))
              ($branches (depth-terms->datums (+ $depth 2) $cases))
              `(let ((,$value-var (cdr ,$var)))
                ,(case $size
                  ((2) `(if (car ,$var) ,(car $branches) ,(cadr $branches)))
                  (else `(index-switch (car ,$var) ,@$branches))))))))))

  (define (depth-choice-type->datum $depth $choice-type)
    `(choice-type
      (list ,@(depth-terms->datums $depth (choice-type-types $choice-type)))))

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
        ((_ (name item ...))
          #`(tuple (quote name) (list item ...))))))

  (define-syntax tuple-type!
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name arg ...))
          #`(tuple-type (quote name) (list arg ...))))))

  (define-syntax choice-type!
    (lambda (stx)
      (syntax-case stx ()
        ((_ arg ...)
          #`(choice-type (list arg ...))))))

  (define-syntax typed!
    (lambda (stx)
      (syntax-case stx ()
        ((_ literal)
          (switch (syntax->datum #`literal)
            ((boolean? $boolean) #`(typed #,$boolean boolean!))
            ((number? $number) #`(typed #,$number number!))
            ((string? $string) #`(typed #,$string string!)))))))

  (define-syntax typed-tuple!
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name item ...))
          #`(typed-tuple (quote name) (list item ...))))))

  (define-syntax typed-function!
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name param ...) body)
          #`(typed-function (quote name) (list param ...) body)))))

  ; -----------------------------------------------

  (define (eval-term $term $env)
    (eval (term->datum $term) $env))
)
