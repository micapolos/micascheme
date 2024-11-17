(import (micalog verilog))

(verilog
  (reg half-clock bit 0)

  (reg counter-8 (vector bit 8) 0)

  (wire counter-4 (vector bit 4)
    (ref counter-8 (3 0)))

  (always (positive-edge clock)
    (set! half-clock (inv half-clock)))

  (always (positive-edge half-clock)
    (if write?
      (set! counter-8 (+ counter-8 1)))))

