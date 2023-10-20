(library (tico compiler)
  (export
    variable variable? variable-type
    abstraction abstraction? abstraction-params abstraction-body
    application application? application-target application-args
    native native? native-typ native-datum

    native-type native-type?
    struct struct? struct-name struct-items
    function-type function-type? function-type-params function-type-result

    compiled compiled? compiled-typed compiled-free-variable-count
    typed typed? typed-type typed-value
    binding binding? binding-type binding-symbol

    bindings-term->compiled
    term->compiled)
  (import (micascheme))

  (data (variable type))
  (data (abstraction params body))
  (data (application target args))
  (data (native typ datum))

  (data (native-type))
  (data (struct name items))
  (data (function-type params result))

  (data (compiled typed free-variable-count))
  (data (typed type value))
  (data (binding symbol type))

  (define (type-matches-binding? $type $binding)
    (equal? $type (binding-type $binding)))

  (define (term->compiled $term)
    (bindings-term->compiled (stack) $term))

  (define (bindings-term->compiled $bindings $term)
    (switch $term
      ((native? $native)
        (compiled
          (typed
            (native-typ $native)
            (native-datum $native))
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
      ((native-type? _) #t)
      ((struct? $struct)
        (exists type-dynamic? (struct-items $struct)))
      ((function-type? $function-type)
        (type-dynamic? (function-type-result $function-type)))
      ((else $other) #t)))

  ; --- generate-symbol ---

  (define (types->generate-symbols $types)
    (map type->generate-symbol $types))

  (define (type->generate-symbol $type)
    (generate-symbol))

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
                  (binding-symbol $binding))
                $index))
            (else
              (bindings-variable-index->compiled $bindings $variable $index)))))))

  ; --- function ---

  (define (bindings-abstraction->bind-compiled $bindings $abstraction $bindings-term->compiled)
    (lets
      ($params (abstraction-params $abstraction))
      ($arity (length $params))
      ($symbols (types->generate-symbols $params))
      ($compiled-body
        ($bindings-term->compiled
          (push-list $bindings (map binding $symbols $params))
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
)
