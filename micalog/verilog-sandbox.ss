(import (micalog verilog))

(verilog
  (counter
    (vector bit 8)
    (initial 0)
    (on (positive-edge clock)
      (if write?
        (+ counter 1)))))
