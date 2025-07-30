(library (zx-next write)
  (export
    write-init
    write-char
    write-string
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

  (define-fragment write-newline
    (ld a #x0d)
    (jp write-char))
)
