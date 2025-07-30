(import
  (zx-next core)
  (zx-next terminal)
  (zx-next write)
  (zx-next scheme alloc)
  (zx-next scheme write)
  (zx-next scheme value)
  (zx-next debug))

(define-fragments
  (hello-world-symbol
    (align 4)
    (db 0)
    (dz "hello-world")))

(run
  (call terminal-init)

  (value-false)
  (call scheme-write)
  (call write-newline)

  (value-true)
  (call scheme-write)
  (call write-newline)

  (value-null)
  (call scheme-write)
  (call write-newline)

  (value-byte #x12)
  (call scheme-write)
  (call write-newline)

  (value-word #x1234)
  (call scheme-write)
  (call write-newline)

  (value-char #\A)
  (call scheme-write)
  (call write-newline)

  (ld a #x12)
  (value-load-byte)
  (call scheme-write)
  (call write-newline)

  (ld hl #x1234)
  (value-load-word)
  (call scheme-write)
  (call write-newline)

  (ld a #\A)
  (value-load-char)
  (call scheme-write)
  (call write-newline)

  ; symbol
  (ld d #x0000)
  (ld hl (fxior hello-world-symbol #x01))  ; why value-tag-symbol does not work?
  (call scheme-write)
  (call write-newline)

  (call loop-bars))

