(import (scheme) (emu zx-spectrum-48 machine) (emu run))

(let ()
  (define-zx-spectrum-48 zx zx-init zx-step)
  (zx-init)
  (time (run zx-step 35000000)))
