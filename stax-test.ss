(import (micascheme) (stax))

(lets
  ($stax1 (make-stax 16))
  ($stax2 (make-stax 16))
  (run
    (stax-u8-push $stax1 12)
    (stax-u8-push $stax1 134)
    (stax-u8-push $stax2 12)
    (stax-u8-push $stax2 134)
    (check (equal? $stax1 $stax2))))
