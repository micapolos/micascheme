(library (emu run)
  (export run)
  (import (scheme) (syntax))

  (define-rule-syntax (run $step $cycles)
    (do
      (($i $cycles (fx-/wraparound $i 1)))
      ((fxzero? $i) (void))
      ($step)))
)
