(library (zx-next writer)
  (export
    write-char
    write-string
    write-newline)
  (import (zx-next core))

  ; writer is an address of write-char proc

  (define-fragment write-char
    (input (hl writer) (a char))
    (jp (hl)))

  (define-fragment write-string
    (input (hl writer) (de string))
    (loop
      (ld a (de))
      (inc de)
      (or a)
      (ret z)
      (preserve (de hl) (call write-char))))

  (define-fragment write-newline
    (input (hl writer))
    (ld a #x0d)
    (jp write-char))
)
