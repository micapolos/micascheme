(import (micalog verilog))

(verilog
  (counter
    (vector bit 8)
    (initial 128)
    (on (positive-edge clock) next-counter))
  (next-value
    (vector bit 8)
    (+ counter 1))
  (next-counter
    (vector bit 8)
    (on (negative-edge clock) next-value)))
