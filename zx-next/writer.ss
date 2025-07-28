(library (zx-next writer)
  (export write-char write-string-m)
  (import (zx-next core))

  ; write is an address of write-char proc

  (block write-char
    (input (hl writer-proc) (a char))
    (jp (hl)))

  (block write-string
    (input (hl writer) (de string))
    (loop
      (ld a (de))
      (inc de)
      (or a)
      (ret z)
      (preserve (de hl) (call write-char))))
)
