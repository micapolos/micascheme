(import (micalog verilog))

(verilog
  (half-clock bit
    (initial 0)
    (on (positive-edge clock)
      (not half-clock)))
  (counter (vector bit 8)
    (initial 0)
    (on (positive-edge half-clock)
      (if write? (+ counter 1)))))
