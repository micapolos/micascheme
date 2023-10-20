(library (tico compiler)
  (export
    variable variable? variable-type
    abstraction abstraction? abstraction-params abstraction-body
    application application? application-target application-args
    native native? native-typ native-datum

    native-type native-type?
    struct struct? struct-name struct-items
    function-type function-type? function-type-params function-type-result

    thunk thunk? thunk-datum thunk-free-variable-count
    compiled compiled? compiled-type compiled-thunk
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

  (data (thunk datum free-variable-count))
  (data (compiled type thunk))
  (data (binding symbol type))

  (define (type-matches-binding? $type $binding)
    (equal? $type (binding-type $binding)))

  (define (term->compiled $term)
    (bindings-term->compiled (stack) $term))

  (define (bindings-term->compiled $bindings $term)
    (switch $term
      ((native? $native)
        (compiled
          (native-typ $native)
          (thunk (native-datum $native) 0)))
      ((variable? $variable)
        (bindings-variable-index->compiled $bindings $variable 0))
      ((abstraction? $abstraction)
        (lets
          ($params (abstraction-params $abstraction))
          ($arity (length $params))
          ($symbols (types->generate-symbols $params))
          ($compiled-body
            (bindings-term->compiled
              (push-list $bindings (map binding $symbols $params))
              (abstraction-body $abstraction)))
          ($body-thunk (compiled-thunk $compiled-body))
          (compiled
            (function-type $params (compiled-type $compiled-body))
            (thunk
              `(lambda (,@$symbols) ,(thunk-datum $body-thunk))
              (max
                (- (thunk-free-variable-count $body-thunk) $arity)
                0)))))
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
                (binding-type $binding)
                (thunk (binding-symbol $binding) $index)))
            (else
              (bindings-variable-index->compiled $bindings $variable $index)))))))

  ; --- apply ---

  (define (compiled-apply $target $args)
    (compiled
      (type-apply
        (compiled-type $target)
        (map compiled-type $args))
      (thunk-apply
        (compiled-thunk $target)
        (map compiled-thunk $args))))

  (define (type-apply $target $args)
    (lets
      ($function-type (ensure function-type? $target))
      (do (unless (for-all equal? (function-type-params $function-type) $args)
        (throw function-apply)))
      (function-type-result $function-type)))

  (define (thunk-apply $target $args)
    (thunk
      (datum-apply
        (thunk-datum $target)
        (map thunk-datum $args))
      (apply max
        (thunk-free-variable-count $target)
        (map thunk-free-variable-count $args))))

  (define (datum-apply $target $args)
    `(,$target ,@$args))

  ; --- struct ---

  (define (compiled-struct $name $items)
    (compiled
      (struct $name (map compiled-type $items))
      (thunk-struct (map compiled-thunk $items))))

  (define (thunk-struct $items)
    (thunk
      `(list ,@(map thunk-datum $items))
      (apply max (map thunk-free-variable-count $items))))
)
