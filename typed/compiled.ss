(library (typed compiled)
  (export
    compiled compiled? compiled-bindings compiled-datum
    value-compiled
    compiled-value
    compiled+value)
  (import
    (micascheme)
    (evaluator)
    (generate))

  (data (compiled bindings datum))

  (define (value-compiled $value)
    (lets
      ($symbol (generate-symbol))
      (compiled
        (stack (cons $symbol $value))
        $symbol)))

  (define (compiled-value $environment $compiled)
    (evaluate
      (evaluator
        $environment
        (compiled-bindings $compiled))
      (compiled-datum $compiled)))

  (define (compiled+value $compiled $symbol $value)
    (compiled
      (push
        (compiled-bindings $compiled)
        (cons $symbol $value))
      (compiled-datum $compiled)))
)
