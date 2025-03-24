(library (typed compiled)
  (export
    compiled compiled? compiled-bindings compiled-datum
    combo-compiled
    compiled-combo
    combine-compiled-list)
  (import
    (micascheme)
    (evaluator)
    (generate)
    (typed combo))

  (data (compiled bindings datum))

  (define (combo-compiled $combo)
    (compiled (stack) (combo-datum $combo)))

  (define (compiled-combo $environment $compiled)
    (combo
      (evaluate
        (evaluator
          $environment
          (map-with
            ($binding (compiled-bindings $compiled))
            (cons (car $binding) (combo-value (cdr $binding)))))
        (compiled-datum $compiled))
      `(lets
        ,@(map-with
          ($binding (compiled-bindings $compiled))
          (list (car $binding) (combo-datum (cdr $binding))))
        ,(compiled-datum $compiled))))

  (define (combine-compiled-list $compiled-list $combine-datums)
    (compiled
      (apply append (reverse (map compiled-bindings $compiled-list)))
      ($combine-datums (map compiled-datum $compiled-list))))
)
