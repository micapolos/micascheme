(library (term)
  (export
    native native? native-term

    function function? function-arity function-body
    function-type function-type? function-type-name function-type-params function-type-result

    application application? application-fn application-args
    use use!
    
    boolean-type boolean-type?
    number-type number-type?
    string-type string-type?

    pair pair-first pair-second
    vector-get

    conditional conditional? conditional-condition conditional-consequent conditional-alternate
    branch branch? branch-index branch-cases branch!

    tuple-type tuple-type? tuple-type-name tuple-type-types
    choice-type choice-type? choice-type-types
    universe universe? universe-depth
    
    term->datum term-eval

    application! function-type! tuple-type! choice-type!
    boolean! number! string! type!)

  (import (micascheme) (variable))

  (data (native term))

  (data (application fn args))
  (data (function arity body))

  (data (boolean-type))
  (data (number-type))
  (data (string-type))
  (data (universe depth))

  (data (conditional condition consequent alternate))

  (data (function-type name params result))

  (data (branch index cases))

  (define pair cons)
  (data (pair-first pair))
  (data (pair-second pair))

  (data (vector-get vector index))

  (data (tuple-type name types))

  (data (choice-type types))
  
  ; --------------------------------------------------

  (define (use $args $body)
    (application 
      (function (length $args) $body)
      $args))

  ; --------------------------------------------------

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
      ((conditional? $conditional) (depth-conditional->datum $depth $conditional))
      ((function? $function) (depth-function->datum $depth $function))
      ((function-type? $function-type) (depth-function-type->datum $depth $function-type))
      ((branch? $branch) (depth-branch->datum $depth $branch))
      ((pair? $pair) (depth-pair->datum $depth $pair))
      ((pair-first? $pair-first) (depth-pair-first->datum $depth $pair-first))
      ((pair-second? $pair-second) (depth-pair-second->datum $depth $pair-second))
      ((vector? $vector) (depth-vector->datum $depth $vector))
      ((vector-get? $vector-get) (depth-vector-get->datum $depth $vector-get))
      ((tuple-type? $tuple-type) (depth-tuple-type->datum $depth $tuple-type))
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

  (define (depth-conditional->datum $depth $conditional)
    `(if
      ,(depth-term->datum $depth (conditional-condition $conditional))
      ,(depth-term->datum $depth (conditional-consequent $conditional))
      ,(depth-term->datum $depth (conditional-alternate $conditional))))

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

  (define (depth-branch->datum $depth $branch)
    `(index-switch 
      ,(depth-term->datum $depth (branch-index $branch))
      ,@(depth-terms->datums $depth (branch-cases $branch))))

  (define (depth-vector->datum $depth $vector)
    `(vector ,@(depth-terms->datums $depth (vector->list $vector))))

  (define (depth-vector-get->datum $depth $vector-get)
    `(vector-ref 
      ,(depth-term->datum $depth (vector-get-vector $vector-get))
      ,(depth-term->datum $depth (vector-get-index $vector-get))))

  (define (depth-pair->datum $depth $pair)
    `(cons 
      ,(depth-term->datum $depth (car $pair))
      ,(depth-term->datum $depth (cdr $pair))))

  (define (depth-pair-first->datum $depth $pair-first)
    `(car ,(depth-term->datum $depth (pair-first-pair $pair-first))))

  (define (depth-pair-second->datum $depth $pair-second)
    `(cdr ,(depth-term->datum $depth (pair-second-pair $pair-second))))

  (define (depth-tuple-type->datum $depth $tuple-type)
    `(tuple-type
      (quote ,(tuple-type-name $tuple-type))
      (list
        ,@(depth-terms->datums
          $depth
          (tuple-type-types $tuple-type)))))

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

  (define boolean! (boolean-type))
  (define number! (number-type))
  (define string! (string-type))
  (define type! (universe 0))

  (define-syntax-rule (application! fn arg ...)
    (application fn (list arg ...)))

  (define-syntax-rule (use! (arg ...) body)
    (use (list arg ...) body))

  (define-syntax-rule (function-type! (name arg ...) result)
    (function-type (quote name) (list arg ...) result))

  (define-syntax-rule (tuple-type! (name arg ...))
    (tuple-type (quote name) (list arg ...)))

  (define-syntax-rule (choice-type! arg ...)
    (choice-type (list arg ...)))

  (define-syntax-rule (branch! index case ...)
    (branch index (list case ...)))

  ; -----------------------------------------------

  (define (term-eval $term $env)
    (eval (term->datum $term) $env))
)
