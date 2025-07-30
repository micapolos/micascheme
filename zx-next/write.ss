(library (zx-next write)
  (export
    write-init
    write-char
    write-string
    write-nibble
    write-byte
    write-word
    write-newline)
  (import (zx-next core))

  (define-fragment write-init
    (input (hl write-char-address))
    (ex de hl)
    (ld hl (+ write-char 1))
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (ret))

  (define-fragment write-char
    (input (a char))
    (jp #x0008))

  (define-fragment write-string
    (input (hl string))
    (loop
      (ld a (hl))
      (inc hl)
      (or a)
      (ret z)
      (preserve (hl) (call write-char))))

  (define-fragment write-nibble
    (input (a nibble))
    (cp 10)
    (jp c digit)
    (add (- #\A 10))
    (jp write)
    digit
    (add #\0)
    write
    (jp write-char))

  (define-fragment write-byte
    (input (a byte))
    (preserve (af)
      (swapnib)
      (and #x0f)
      (call write-nibble))
    (and #x0f)
    (jp write-nibble))

  (define-fragment write-word
    (input (hl word))
    (preserve (hl)
      (ld a h)
      (call write-byte))
    (ld a l)
    (jp write-byte))

  (define-fragment write-newline
    (ld a #x0d)
    (jp write-char))
)
