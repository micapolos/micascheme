(import (micalog verilog))

(verilog
  (half-clock bit
    (on (positive-edge clock)
      (not clock)))
  (counter (vector bit 8)
    (initial 0)
    (on (positive-edge half-clock)
      (if write?
        (+ counter 1)))))
