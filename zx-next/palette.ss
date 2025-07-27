(library (zx-next palette)
  (export palette-load-9bit)
  (import (zx-next core))

  (proc palette-load-9bit
    (input
      (hl palette-addr)
      (b colot-count))
    (loop-djnz
      (dup 2
        (ld a (hl))
        (inc hl)
        (nextreg #x44 a)))
    (ret))
)
