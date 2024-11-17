(import (micalog syntax))

(verilog
  (counter
    (* bit 8)
    (initial 128)
    (on (positive-edge clock) next-counter))
  (next-value
    (* bit 8)
    (+ counter 1))
  (next-counter
    (* bit 8)
    (on (negative-edge clock) next-value)))
