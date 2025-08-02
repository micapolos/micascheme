(library (zx-next panic)
  (export with-panic panic)
  (import (zx-next core))

  (define-ops
    ((with-panic body ...)
      (with-labels (capture-pc no-panic)
        (ld hl no-panic)
        (push hl)
        (ld iy 0)
        (add iy sp)

        body ...

        (rcf)
        no-panic))
    ((panic)
      (ld sp iy)
      (scf)
      (ret)))
)
