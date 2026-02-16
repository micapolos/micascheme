(import (leo2 base) (leo2 term) (leo2 normalize))

(check
  (equal?
    (normalize (stack) (native 0 10 (list)))
    (native 0 10 (stack))))
