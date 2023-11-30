(library (tico resolution)
  (export
    resolution resolution? resolution-depth-opt resolution-bindings
    resolution-promote)
  (import
    (micascheme))

  (data (resolution depth-opt bindings))

  (define (resolution-promote $resolution $size)
    (switch (resolution-depth-opt $resolution)
      ((false? _) $resolution)
      ((else $depth)
        (switch-opt (- $depth $size)
          ((nonnegative-integer? $depth)
            (resolution $depth (resolution-bindings $resolution)))))))
)
