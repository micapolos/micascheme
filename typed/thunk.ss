(library (typed thunk)
  (export
    thunk thunk? thunk-max-index thunk-compiled
    thunk-promote
    combine-thunks)
  (import
    (micascheme)
    (typed compiled))

  (data (thunk max-index compiled))

  (define (thunk-promote $environment (thunk $max-index $compiled) $depth)
    (lets
      ($new-max-index (- $max-index $depth))
      (if (< $new-max-index 0)
        (compiled-value $environment $compiled)
        (thunk $new-max-index $compiled))))

  (define (combine-thunks $thunks $datum-proc)
    (thunk
      (apply max (map thunk-max-index $thunks))
      (combine-compiled-list (map thunk-compiled $thunks) $datum-proc)))
)
