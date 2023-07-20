(library (term)
  (export
    native native? native-value

    variable variable? variable-index v0 v1 v2
    function function? function-arity function-body
    recursive recursive? recursive-function
    function-type function-type? function-type-params function-type-result
    forall forall? forall-arity forall-body

    application application? application-fn application-args
    use use!
    
    boolean-type boolean-type? boolean!
    number-type number-type? number!
    string-type string-type? string!

    term-type term-type? term!
    list-type list-type? list-type-item
    pair-type pair-type? pair-type-first pair-type-second

    pair pair-first pair-second
    vector-get

    conditional conditional? conditional-condition conditional-consequent conditional-alternate
    branch branch? branch-index branch-cases branch!

    tuple-type tuple-type? tuple-type-name tuple-type-types
    choice-type choice-type? choice-type-types
    universe universe? universe-depth type!
    
    term->datum term->syntax term-eval

    application! function-type! tuple-type! choice-type!)

  (import (micascheme))

  (data (native value))

  (data (variable index))

  (data (application fn args))
  (data (function arity body))
  (data (recursive function))

  (data (boolean-type))
  (data (number-type))
  (data (string-type))
  (data (universe depth))
  
  (data (term-type))
  (data (list-type item))
  (data (pair-type first second))

  (data (conditional condition consequent alternate))

  (data (function-type params result))
  (data (forall arity body))

  (data (branch index cases))

  (define pair cons)
  (data (pair-first pair))
  (data (pair-second pair))

  (data (vector-get vector index))

  (data (tuple-type name types))

  (data (choice-type types))
  
  ; --------------------------------------------------

  (define v0 (variable 0))
  (define v1 (variable 1))
  (define v2 (variable 2))

  (define (use $args $body)
    (application 
      (function (length $args) $body)
      $args))

  ; --------------------------------------------------

  (define (term->datum $term)
    (syntax->datum (env-term->syntax (list) $term)))

  (define (term->syntax $term)
    (env-term->syntax (list) $term))

  (define (env-push $env)
    (cons
      (generate-temporary
        (build-identifier ($string #`v)
          (string-append $string (number->string (length $env)))))
      $env))

  (define (env-arity-push $env $arity)
    (iterate env-push $env $arity))

  (define (env-terms->syntaxes $env $terms)
    (map (partial env-term->syntax $env) $terms))

  (define (env-term->syntax $env $term)
    (switch $term
      ((native? $native) (env-native->syntax $env $native))
      ((symbol? $symbol) `(quote ,$symbol))
      ((boolean? $string) $string)
      ((number? $number) $number)
      ((string? $string) $string)
      ((boolean-type? _) #`(boolean-type))
      ((number-type? _) #`(number-type))
      ((string-type? _) #`(string-type))
      ((universe? $universe) #`(universe #,(universe-depth $universe)))
      ((variable? $variable) (env-variable->syntax $env $variable))
      ((application? $application) (env-application->syntax $env $application))
      ((conditional? $conditional) (env-conditional->syntax $env $conditional))
      ((function? $function) (env-function->syntax $env $function))
      ((recursive? $recursive) (env-recursive->syntax $env $recursive))
      ((function-type? $function-type) (env-function-type->syntax $env $function-type))
      ((branch? $branch) (env-branch->syntax $env $branch))
      ((pair? $pair) (env-pair->syntax $env $pair))
      ((pair-first? $pair-first) (env-pair-first->syntax $env $pair-first))
      ((pair-second? $pair-second) (env-pair-second->syntax $env $pair-second))
      ((vector? $vector) (env-vector->syntax $env $vector))
      ((vector-get? $vector-get) (env-vector-get->syntax $env $vector-get))
      ((tuple-type? $tuple-type) (env-tuple-type->syntax $env $tuple-type))
      ((choice-type? $choice-type) (env-choice-type->syntax $env $choice-type))
      ((else _) (throw env-term->syntax $env $term))))

  (define (env-native->syntax $env $native)
    (native-value $native))

  (define (env-variable->syntax $env $variable)
    (list-ref $env (variable-index $variable)))

  (define (env-application->syntax $env $application)
    #`(
      #,(env-term->syntax $env (application-fn $application))
      #,@(env-terms->syntaxes $env (application-args $application))))

  (define (env-conditional->syntax $env $conditional)
    #`(if
      #,(env-term->syntax $env (conditional-condition $conditional))
      #,(env-term->syntax $env (conditional-consequent $conditional))
      #,(env-term->syntax $env (conditional-alternate $conditional))))

  (define (env-function->syntax $env $function)
    (lets
      ($arity (function-arity $function))
      ($env (iterate env-push $env $arity))
      #`(lambda (#,@(map (partial list-ref $env) (reverse (indices $arity))))
        #,(env-term->syntax $env
          (function-body $function)))))

  (define (env-recursive->syntax $env $recursive)
    (lets
      ($function (recursive-function $recursive))
      ($arity (function-arity $function))
      ($env (iterate env-push (env-push $env) $arity))
      #`(rec #,(list-ref $env $arity)
        (lambda (#,@(map (partial list-ref $env) (reverse (indices $arity))))
          #,(env-term->syntax $env
            (function-body $function))))))

  (define (env-function-type->syntax $env $function-type)
    #`(function-type
      (list #,@(env-terms->syntaxes $env (function-type-params $function-type)))
      #,(env-term->syntax $env (function-type-result $function-type))))

  (define (env-branch->syntax $env $branch)
    #`(index-switch
      #,(env-term->syntax $env (branch-index $branch))
      #,@(env-terms->syntaxes $env (branch-cases $branch))))

  (define (env-vector->syntax $env $vector)
    #`(vector #,@(env-terms->syntaxes $env (vector->list $vector))))

  (define (env-vector-get->syntax $env $vector-get)
    #`(vector-ref
      #,(env-term->syntax $env (vector-get-vector $vector-get))
      #,(env-term->syntax $env (vector-get-index $vector-get))))

  (define (env-pair->syntax $env $pair)
    #`(cons
      #,(env-term->syntax $env (car $pair))
      #,(env-term->syntax $env (cdr $pair))))

  (define (env-pair-first->syntax $env $pair-first)
    #`(car #,(env-term->syntax $env (pair-first-pair $pair-first))))

  (define (env-pair-second->syntax $env $pair-second)
    #`(cdr #,(env-term->syntax $env (pair-second-pair $pair-second))))

  (define (env-tuple-type->syntax $env $tuple-type)
    #`(tuple-type
      (quote #,(tuple-type-name $tuple-type))
      (list
        #,@(env-terms->syntaxes
          $env
          (tuple-type-types $tuple-type)))))

  (define (env-choice-type->syntax $env $choice-type)
    #`(choice-type
      (list #,@(env-terms->syntaxes $env (choice-type-types $choice-type)))))

  ; -----------------------------------------------

  (define boolean! (boolean-type))
  (define number! (number-type))
  (define string! (string-type))
  (define type! (universe 0))
  (define term! (term-type))

  (define-syntax-rule (application! fn arg ...)
    (application fn (list arg ...)))

  (define-syntax-rule (use! arg ... body)
    (use (list arg ...) body))

  (define-syntax-rule (function-type! (arg ...) result)
    (function-type (list arg ...) result))

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
