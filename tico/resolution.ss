(library (tico resolution)
  (export
    resolution resolution? resolution-index-opt resolution-bindings
    resolution-promote)
  (import
    (micascheme)
    (tico index)
    (tico arity))

  (data (resolution index-opt bindings))

  (define (resolution-promote $resolution $arity)
    (switch (resolution-index-opt $resolution)
      ((false? _) $resolution)
      ((else $index)
        (switch-opt (- (index-value $index) (arity-value $arity))
          ((nonnegative-integer? $value)
            (resolution
              (index $value)
              (resolution-bindings $resolution)))))))
)
