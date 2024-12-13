(library (tico reference)
  (export
    reference reference? reference-index-opt reference-bindings
    reference-promote
    reference-append)
  (import
    (micascheme)
    (tico index)
    (tico arity))

  (data (reference index-opt bindings))

  (define (reference-promote $reference $arity)
    (switch (reference-index-opt $reference)
      ((false? _) $reference)
      ((else $index)
        (switch? (- (index-value $index) (arity-value $arity))
          ((nonnegative-integer? $value)
            (reference
              (index $value)
              (reference-bindings $reference)))))))

  (define (reference-append . $references)
    (reference
      (lets
        ($indices (map reference-index-opt $references))
        (and
          (for-all not-false? $indices)
          (index (apply max (map index-value $indices)))))
      (apply append (map reference-bindings $references))))
)
