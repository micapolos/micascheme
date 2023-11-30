(import (micascheme) (tico sizing))

(lets
  ($sizing (sizing 3 "foo"))
  (run
    (check (sizing? $sizing))
    (check (not (sizing? "foo")))
    (check (equal? (sizing-size $sizing) 3))
    (check (equal? (sizing-value $sizing) "foo"))))
