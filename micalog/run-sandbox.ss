(import (micalog run) (micalog std))

(micalog-run
  (module sandbox
    (input 1 reset?)
    (output exit? reset?)))
