(library (term)
  (export
    native native? native-term

    function function? function-arity function-body
    recursive recursive? recursive-function
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
    
    term->datum term->syntax term-eval

    application! function-type! tuple-type! choice-type!
    boolean! number! string! type!)

  (import (micascheme) (variable))

  (data (native term))

  (data (application fn args))
  (data (function arity body))
  (data (recursive function))

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
    (syntax->datum (depth-term->syntax 0 $term)))

  (define (term->syntax $term)
    (depth-term->syntax 0 $term))

  (define (depth-terms->syntaxes $depth $terms)
    (map (partial depth-term->syntax $depth) $terms))

  (define (depth-term->syntax $depth $term)
    (switch $term
      ((native? $native) (depth-native->syntax $depth $native))
      ((symbol? $symbol) `(quote ,$symbol))
      ((boolean? $string) $string)
      ((number? $number) $number)
      ((string? $string) $string)
      ((boolean-type? _) #`(boolean-type))
      ((number-type? _) #`(number-type))
      ((string-type? _) #`(string-type))
      ((universe? $universe) #`(universe #,(universe-depth $universe)))
      ((variable? $variable) (depth-variable->syntax $depth $variable))
      ((application? $application) (depth-application->syntax $depth $application))
      ((conditional? $conditional) (depth-conditional->syntax $depth $conditional))
      ((function? $function) (depth-function->syntax $depth $function))
      ((recursive? $recursive) (depth-recursive->syntax $depth $recursive))
      ((function-type? $function-type) (depth-function-type->syntax $depth $function-type))
      ((branch? $branch) (depth-branch->syntax $depth $branch))
      ((pair? $pair) (depth-pair->syntax $depth $pair))
      ((pair-first? $pair-first) (depth-pair-first->syntax $depth $pair-first))
      ((pair-second? $pair-second) (depth-pair-second->syntax $depth $pair-second))
      ((vector? $vector) (depth-vector->syntax $depth $vector))
      ((vector-get? $vector-get) (depth-vector-get->syntax $depth $vector-get))
      ((tuple-type? $tuple-type) (depth-tuple-type->syntax $depth $tuple-type))
      ((choice-type? $choice-type) (depth-choice-type->syntax $depth $choice-type))
      ((else _) (throw depth-term->syntax $depth $term))))

  (define (depth-native->syntax $depth $native)
    (native-term $native))

  (define (depth-variable->syntax $depth $variable)
    (let (($index (- $depth (variable-index $variable) 1)))
      (if (< $index 0) 
        (throw depth-variable->syntax $depth $variable)
        (depth->syntax $index))))

  (define (depth-application->syntax $depth $application)
    #`(
      #,(depth-term->syntax $depth (application-fn $application))
      #,@(depth-terms->syntaxes $depth (application-args $application))))

  (define (depth-conditional->syntax $depth $conditional)
    #`(if
      #,(depth-term->syntax $depth (conditional-condition $conditional))
      #,(depth-term->syntax $depth (conditional-consequent $conditional))
      #,(depth-term->syntax $depth (conditional-alternate $conditional))))

  (define (depth-function->syntax $depth $function)
    (lets
      ($arity (function-arity $function))
      #`(lambda (#,@(depth-size->syntaxes $depth $arity))
        #,(depth-term->syntax
          (+ $depth $arity)
          (function-body $function)))))

  (define (depth-recursive->syntax $depth $recursive)
    (lets
      ($function (recursive-function $recursive))
      ($arity (function-arity $function))
      ($body-depth (+ $depth 1))
      #`(rec #,(depth->syntax $depth)
        (lambda (#,@(depth-size->syntaxes $body-depth $arity))
          #,(depth-term->syntax
            (+ $body-depth $arity)
            (function-body $function))))))

  (define (depth-function-type->syntax $depth $function-type)
    #`(function-type
      (quote #,(function-type-name $function-type))
      (list #,@(depth-terms->syntaxes $depth (function-type-params $function-type)))
      #,(depth-term->syntax $depth (function-type-result $function-type))))

  (define (depth-branch->syntax $depth $branch)
    #`(index-switch
      #,(depth-term->syntax $depth (branch-index $branch))
      #,@(depth-terms->syntaxes $depth (branch-cases $branch))))

  (define (depth-vector->syntax $depth $vector)
    #`(vector #,@(depth-terms->syntaxes $depth (vector->list $vector))))

  (define (depth-vector-get->syntax $depth $vector-get)
    #`(vector-ref
      #,(depth-term->syntax $depth (vector-get-vector $vector-get))
      #,(depth-term->syntax $depth (vector-get-index $vector-get))))

  (define (depth-pair->syntax $depth $pair)
    #`(cons
      #,(depth-term->syntax $depth (car $pair))
      #,(depth-term->syntax $depth (cdr $pair))))

  (define (depth-pair-first->syntax $depth $pair-first)
    #`(car #,(depth-term->syntax $depth (pair-first-pair $pair-first))))

  (define (depth-pair-second->syntax $depth $pair-second)
    #`(cdr #,(depth-term->syntax $depth (pair-second-pair $pair-second))))

  (define (depth-tuple-type->syntax $depth $tuple-type)
    #`(tuple-type
      (quote #,(tuple-type-name $tuple-type))
      (list
        #,@(depth-terms->syntaxes
          $depth
          (tuple-type-types $tuple-type)))))

  (define (depth-choice-type->syntax $depth $choice-type)
    #`(choice-type
      (list #,@(depth-terms->syntaxes $depth (choice-type-types $choice-type)))))

  (define (depth->syntax $depth)
    (datum->syntax #`depth->syntax
      (string->symbol
        (string-append "v"
          (number->string $depth)))))

  (define (depth-size->syntaxes $depth $size)
    (map depth->syntax
      (map (partial + $depth)
        (indices $size))))

  ; -----------------------------------------------

  (define boolean! (boolean-type))
  (define number! (number-type))
  (define string! (string-type))
  (define type! (universe 0))

  (define-syntax-rule (application! fn arg ...)
    (application fn (list arg ...)))

  (define-syntax-rule (use! arg ... body)
    (use (list arg ...) body))

  (define-syntax-rule (function-type! (name arg ...) result)
    (function-type (quote name) (list arg ...) result))

  (define-syntax-rule (tuple-type! name arg ...)
    (tuple-type (quote name) (list arg ...)))

  (define-syntax-rule (choice-type! arg ...)
    (choice-type (list arg ...)))

  (define-syntax-rule (branch! index case ...)
    (branch index (list case ...)))

  ; -----------------------------------------------

  (define (term-eval $term $env)
    (eval (term->datum $term) $env))
)
