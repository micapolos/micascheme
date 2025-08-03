(library (zx-next throw)
  (export throw catch)
  (import (zx-next core))

  (define-ops
    ((catch body ...)
      (with-labels (end)
        (preserve (iy)
          ; push catch return address on the stack
          (ld hl end)
          (push hl)

          ; save catch stack pointer in IY
          (ld iy 0)
          (add iy sp)

          ; Execute catch body
          body ...

          ; Restore state after normal execution
          (pop hl)
          (rcf)  ; reset carry flag on no-throw

          end)))

    ((throw)
      (ld sp iy)
      (scf)  ; set carry flag on throw
      (ret)))
)
