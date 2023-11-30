(library (tico reference)
  (export
    reference reference? reference-index-opt reference-bindings
    reference-promote)
  (import
    (micascheme)
    (tico index)
    (tico arity))

  (data (reference index-opt bindings))

  (define (reference-promote $reference $arity)
    (switch (reference-index-opt $reference)
      ((false? _) $reference)
      ((else $index)
        (switch-opt (- (index-value $index) (arity-value $arity))
          ((nonnegative-integer? $value)
            (reference
              (index $value)
              (reference-bindings $reference)))))))
)
