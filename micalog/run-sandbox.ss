(import (micalog run) (micalog std))

(micalog-run
  (module sandbox
    (input clock)
    (input 1 reset?)
    (register 4 counter)
    (on (posedge clock)
      (cond
        (reset?
          (set counter 10))
        (else
          (log counter counter)
          (dec counter))))
    (output exit? (= counter 0))))
