(library (typed thunk)
  (export
    thunk thunk? thunk-max-index thunk-compiled
    thunk-promote
    thunk-bind)
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

  (define (thunk-bind $thunk $value $datum-proc)
    (thunk
      (thunk-max-index $thunk)
      (compiled-bind (thunk-compiled $thunk) $value $datum-proc)))
)
