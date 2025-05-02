(import (micascheme) (vm vm))

(check
  (equal?
    (make-empty-vm 1024)
    (make-vm (make-bytevector 1024 0) 1024)))


