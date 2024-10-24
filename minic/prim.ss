(library (minic prim)
  (export
    prim-zero?
    prim+ prim+1
    prim- prim-1
    prim-ref prim-set!
    prim-switch)
  (import (scheme) (syntaxes) (switch))

  (define-rules-syntaxes
    ((prim-zero? $a) (($primitive 3 fxzero?) $a))
    ((prim+ $a $b) (($primitive 3 fx+/wraparound) $a $b))
    ((prim+1 $a) (prim+ $a 1))
    ((prim- $a $b) (($primitive 3 fx-/wraparound) $a $b))
    ((prim-1 $a) (prim- $a 1))
    ((prim-ref $vec $idx) (($primitive 3 fxvector-ref) $vec $idx))
    ((prim-set! $vec $idx $val) (($primitive 3 fxvector-set!) $vec $idx $val))
    ; TODO: Optimize this one, using if, case, or table depending on the number of branches.
    ((prim-switch $index $branch ...) (index-switch $index $branch ...)))
)
