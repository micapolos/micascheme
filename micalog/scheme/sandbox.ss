(import (micalog run) (micalog std))

(micalog-run
  (module colour-bars
    (input clock)

    (on (posedge clock) (log tick clock))
    (on (negedge clock) (log tack clock))))
