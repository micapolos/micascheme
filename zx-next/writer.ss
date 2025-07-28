(library (zx-next writer)
  (export
    write-char
    write-string
    newline)
  (import (zx-next core))

  ; writer is an address of write-char proc

  (define-fragments
    (write-char
      (input
        (hl writer)
        (a char))
      (jp (hl)))
    (write-string
      (input
        (hl writer)
        (de string))
      (loop
        (ld a (de))
        (inc de)
        (or a)
        (ret z)
        (preserve (de hl)
          (call write-char))))
    (newline
      (ld a #x0d)
      (jp write-char)))
)
