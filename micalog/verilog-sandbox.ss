(import (micalog verilog))

(verilog
  (half-clock bit
    (initial 0)
    (on (positive-edge clock)
      (inv half-clock)))
  (counter-8 (vector bit 8)
    (initial 0)
    (on (positive-edge half-clock)
      (if write? (+ counter 1))))
  (counter-4 (vector bit 4)
    (ref counter-8 (3 0))))
