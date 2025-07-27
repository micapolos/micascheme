(import (asm base) (asm size-address))

(check
  (equal?
    (sizes->addresses (list 2 4 5))
    (list 0 2 6)))
