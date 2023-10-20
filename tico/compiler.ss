(library (tico compiler)
  (export
    variable variable? variable-type
    abstraction abstraction? abstraction-params abstraction-body
    application application? application-target application-args
    native native? native-type native-value

    struct struct? struct-name struct-items
    function-type function-type? function-type-params function-type-result

    compiled compiled? compiled-typed compiled-free-variable-count
    typed typed? typed-type typed-value
    binding binding? binding-type binding-value
    thunk thunk? thunk-value thunk-free-variable-count

    bindings-term->compiled
    term->compiled)
  (import
    (micascheme)
    (evaluator))

  (data (variable type))
  (data (abstraction params body))
  (data (application target args))
  (data (native type value))

  (data (struct name items))
  (data (function-type params result))

  (data (compiled typed free-variable-count))
  (data (thunk value free-variable-count))
  (data (typed type value))
  (data (binding type value))

  (data (evaluated value))

  (define (type-matches-binding? $type $binding)
    (equal? $type (binding-type $binding)))

  (define (term->compiled $term)
    (bindings-term->compiled (stack) $term))

  (define (bindings-term->compiled $bindings $term)
    (switch $term
      ((native? $native)
        (compiled
          (typed
            (native-type $native)
            (native-value $native))
          0))
      ((variable? $variable)
        (bindings-variable-index->compiled $bindings $variable 0))
      ((abstraction? $abstraction)
        (bindings-abstraction->bind-compiled $bindings $abstraction bindings-term->compiled))
      ((application? $application)
        (compiled-apply
          (bindings-term->compiled $bindings (application-target $application))
          (map (partial bindings-term->compiled $bindings) (application-args $application))))
      ((struct? $struct)
        (compiled-struct
          (struct-name $struct)
          (map
            (partial bindings-term->compiled $bindings)
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

  (define (bindings-variable->compiled $bindings $variable)
    (bindings-variable-index->compiled $bindings $variable 0))

  (define (bindings-variable-index->compiled $bindings $variable $index)
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
              (compiled
                (typed
                  (binding-type $binding)
                  (binding-value $binding))
                $index))
            (else
              (bindings-variable-index->compiled $bindings $variable $index)))))))

  ; --- abstraction ---

  (define (bindings-abstraction->bind-compiled $bindings $abstraction $bindings-term->compiled)
    (lets
      ($params (abstraction-params $abstraction))
      ($arity (length $params))
      ($symbols (generate-symbols $arity))
      ($compiled-body
        ($bindings-term->compiled
          (push-list $bindings (map binding $params $symbols))
          (abstraction-body $abstraction)))
      ($typed-body (compiled-typed $compiled-body))
      (compiled
        (typed
          (function-type $params (typed-type $typed-body))
          `(lambda (,@$symbols) ,(typed-value $typed-body)))
        (max
          (- (compiled-free-variable-count $compiled-body) $arity)
          0))))

  ; --- apply ---

  (define (compiled-apply $target $args)
    (compiled
      (typed-apply
        (compiled-typed $target)
        (map compiled-typed $args))
      (apply max
        (compiled-free-variable-count $target)
        (map compiled-free-variable-count $args))))

  (define (type-apply $target $args)
    (lets
      ($function-type (ensure function-type? $target))
      (do (unless (for-all equal? (function-type-params $function-type) $args)
        (throw function-apply)))
      (function-type-result $function-type)))

  (define (typed-apply $target $args)
    (typed
      (type-apply
        (typed-type $target)
        (map typed-type $args))
      `(
        ,(typed-value $target)
        ,@(map typed-value $args))))

  ; --- struct ---

  (define (compiled-struct $name $items)
    (lets
      ($typed-items (map compiled-typed $items))
      (compiled
        (typed
          (struct $name (map typed-type $typed-items))
          `(list ,@(map typed-value $typed-items)))
        (apply max (map compiled-free-variable-count $items)))))

  ; --- evaluation ---

  (define (bindings-term->evaluate $evaluator-bindings $compiler-bindings $term)
    (switch $term
      ((native? $native)
        (typed
          (native-type $native)
          (evaluated
            (bindings-datum->value $evaluator-bindings (native-value $native)))))
      ((variable? $variable)
        (lets
          ($compiled (bindings-variable->compiled $compiler-bindings $variable))
          ($typed (compiled-typed $compiled))
          ($type (typed-type $typed))
          ($datum (typed-value $typed))
          ($free-variable-count (compiled-free-variable-count $compiled))
          (typed $type
            (cond
              ((= (compiled-free-variable-count $compiled) 0)
                (bindings-datum->value $evaluator-bindings $datum))
              (else
                (thunk $datum $free-variable-count))))))
      ((application? $application)
        (lets
          ($typed-evaluated-target
            (bindings-term->evaluate $evaluator-bindings $compiler-bindings
              (application-target $application)))
          ($typed-evaluated-args
            (map
              (partial bindings-term->evaluate $evaluator-bindings $compiler-bindings)
              (application-args $application)))
          (todo)))
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
)
