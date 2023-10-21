(library (tico compiler)
  (export
    variable variable? variable-type
    abstraction abstraction? abstraction-params abstraction-body
    application application? application-target application-args
    native native? native-type native-value

    struct struct? struct-name struct-items
    function-type function-type? function-type-params function-type-result

    typed typed? typed-type typed-value
    binding binding? binding-type binding-value
    thunk thunk? thunk-value thunk-free-variable-count
    constant constant? constant-value

    bindings-term->typed-thunk
    term->typed-thunk)
  (import
    (micascheme)
    (evaluator))

  (data (variable type))
  (data (abstraction params body))
  (data (application target args))
  (data (native type value))

  (data (struct name items))
  (data (function-type params result))

  (data (thunk value free-variable-count))
  (data (constant value))
  (data (typed type value))
  (data (binding type value))

  (define (type-matches-binding? $type $binding)
    (equal? $type (binding-type $binding)))

  (define (term->typed-thunk $term)
    (bindings-term->typed-thunk (stack) $term))

  (define (bindings-term->typed-thunk $bindings $term)
    (switch $term
      ((native? $native)
        (typed
          (native-type $native)
          (thunk (native-value $native) 0)))
      ((variable? $variable)
        (bindings-variable-index->typed-thunk $bindings $variable 0))
      ((abstraction? $abstraction)
        (bindings-abstraction->typed-thunk $bindings $abstraction))
      ((application? $application)
        (typed-thunk-apply
          (bindings-term->typed-thunk $bindings (application-target $application))
          (map (partial bindings-term->typed-thunk $bindings) (application-args $application))))
      ((struct? $struct)
        (typed-thunk-struct
          (struct-name $struct)
          (map
            (partial bindings-term->typed-thunk $bindings)
            (struct-items $struct))))
      ((else $other)
        (throw not-term $other))))

  (define (type-dynamic? $type)
    (switch $type
      ((struct? $struct)
        (exists type-dynamic? (struct-items $struct)))
      ((function-type? $function-type)
        (type-dynamic? (function-type-result $function-type)))
      ((else $other) #t)))

  ; --- variable ---

  (define (bindings-variable->typed-thunk $bindings $variable)
    (bindings-variable-index->typed-thunk $bindings $variable 0))

  (define (bindings-variable-index->typed-thunk $bindings $variable $index)
    (switch $bindings
      ((null? _)
        (throw not-bound $variable))
      ((else $pair)
        (lets
          ($index (+ $index 1))
          ($binding (car $pair))
          ($bindings (cdr $pair))
          (cond
            ((equal? (variable-type $variable) (binding-type $binding))
              (typed
                (binding-type $binding)
                (thunk (binding-value $binding) $index)))
            (else
              (bindings-variable-index->typed-thunk $bindings $variable $index)))))))

  ; --- abstraction ---

  (define (bindings-abstraction->typed-thunk $bindings $abstraction)
    (lets
      ($params (abstraction-params $abstraction))
      ($arity (length $params))
      ($symbols (generate-symbols $arity))
      ($typed-body-thunk
        (bindings-term->typed-thunk
          (push-list $bindings (map binding $params $symbols))
          (abstraction-body $abstraction)))
      ($body-type (typed-type $typed-body-thunk))
      ($body-thunk (typed-value $typed-body-thunk))
      (typed
        (function-type $params $body-type)
        (thunk
          `(lambda (,@$symbols) ,(thunk-value $body-thunk))
          (max
            (- (thunk-free-variable-count $body-thunk) $arity)
            0)))))

  ; --- apply ---

  (define (typed-thunk-apply $target $args)
    (typed
      (type-apply
        (typed-type $target)
        (map typed-type $args))
      (thunk-apply
        (typed-value $target)
        (map typed-value $args))))

  (define (type-apply $target $args)
    (lets
      ($function-type (ensure function-type? $target))
      (do (unless (for-all equal? (function-type-params $function-type) $args)
        (throw function-apply)))
      (function-type-result $function-type)))

  (define (thunk-apply $target $args)
    (thunk
      (datum-apply
        (thunk-value $target)
        (map thunk-value $args))
      (free-variable-count-apply
        (thunk-free-variable-count $target)
        (map thunk-free-variable-count $args))))

  (define (datum-apply $target $args)
    `(,$target ,@$args))

  (define (free-variable-count-apply $target $args)
    (apply max $target $args))

  ; --- struct ---

  (define (typed-thunk-struct $name $items)
    (lets
      ($types (map typed-type $items))
      ($thunks (map typed-value $items))
      ($datums (map thunk-value $thunks))
      ($free-variable-counts (map thunk-free-variable-count $thunks))
      (typed
        (struct $name $types)
        (thunk
          `(list ,@$datums)
          (apply max $free-variable-counts)))))

  ; --- evaluation ---

  (define (bindings-term->evaluate $evaluator-bindings $compiler-bindings $term)
    (switch $term
      ((native? $native)
        (typed
          (native-type $native)
          (constant
            (bindings-datum->value $evaluator-bindings (native-value $native)))))
      ((variable? $variable)
        (lets
          ($typed-thunk (bindings-variable->typed-thunk $compiler-bindings $variable))
          ($type (typed-type $typed-thunk))
          ($thunk (typed-value $typed-thunk))
          ($datum (thunk-value $thunk))
          ($free-variable-count (thunk-free-variable-count $thunk))
          (typed $type
            (cond
              ((= $free-variable-count 0)
                (bindings-datum->value $evaluator-bindings $datum))
              (else
                (thunk $datum $free-variable-count))))))
      ((application? $application)
        (evaluated-apply
          (bindings-term->evaluate $evaluator-bindings $compiler-bindings
            (application-target $application))
          (map
            (partial bindings-term->evaluate $evaluator-bindings $compiler-bindings)
            (application-args $application))))
      ((else $other)
        (throw invalid-term $other))))

  (define (bindings-typed-datum->typed-value $bindings $typed)
    (typed
      (typed-type $typed)
      (bindings-datum->value $bindings (typed-value $typed))))

  (define (bindings-datum->value $bindings $datum)
    (evaluate
      (evaluator (environment `(micascheme)) $bindings)
      $datum))

  ; --- devaluation ---

  (define (type-value->term $type $value)
    (switch $type
      ((native-type? $native-type)
        (native $value))
      ((struct? $struct)
        (struct
          (struct-name $struct)
          (map type-value->term
            (struct-items $struct)
            $value)))
      ((else $other)
        (throw `invalid-type $type))))

  (define (typed-value->term $typed-value)
    (type-value->term
      (typed-type $typed-value)
      (typed-value $typed-value)))
)
