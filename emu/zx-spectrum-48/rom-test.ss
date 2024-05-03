(import (scheme) (check) (emu zx-spectrum-48 rom))

(let ()
  (define-zx-spectrum-48-rom rom)
  (check (equal? (rom 0) 62)))
