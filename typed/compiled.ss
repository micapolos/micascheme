(library (typed compiled)
  (export
    compiled compiled? compiled-bindings compiled-datum
    value-compiled
    compiled-value
    combine-compiled-list)
  (import
    (micascheme)
    (evaluator)
    (generate))

  (data (compiled bindings datum))

  (define (value-compiled $value)
    (if (literal? $value)
      (compiled (stack) $value)
      (lets
        ($symbol (generate-symbol))
        (compiled
          (stack (cons $symbol $value))
          $symbol))))

  (define (compiled-value $environment $compiled)
    (evaluate
      (evaluator
        $environment
        (compiled-bindings $compiled))
      (compiled-datum $compiled)))

  (define (combine-compiled-list $compiled-list $combine-datums)
    (compiled
      (apply append (reverse (map compiled-bindings $compiled-list)))
      ($combine-datums (map compiled-datum $compiled-list))))
)
