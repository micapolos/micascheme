(library (zx-next throw)
  (export throw catch)
  (import (zx-next core))

  (define-ops
    ((catch body ...)
      (with-labels (end)
        (ld iy end)
        (push iy)
        (ld iy 0)
        (add iy sp)

        body ...

        (rcf)
        end))

    ((throw)
      (ld sp iy)
      (scf)
      (ret)))
)
