(import
  (zx-next core)
  (zx-next terminal)
  (zx-next writer)
  (zx-next debug))

(define-fragments
  (hello-world (dz "Hello, world!\r"))
  (press-space (dz "... Press SPACE ..."))
  (long-line (dz "Hello, this is a very long string which will overflow the terminal by a couple of lines. Really, this is how long it is.")))

(run
  (call terminal-init)

  (ld hl terminal-write-char)
  (ld de hello-world)
  (call write-string)

  (ld hl terminal-write-char)
  (ld de hello-world)
  (call write-string)

  (ld hl terminal-write-char)
  (ld de hello-world)
  (call write-string)

  (ld hl terminal-write-char)
  (ld de hello-world)
  (call write-string)

  (ld hl #x1f00)
  (call terminal-move-to)

  (ld hl terminal-write-char)
  (ld de press-space)
  (call write-string)

  (call wait-space)

  (ld hl terminal-write-char)
  (call write-newline)

  (ld hl terminal-write-char)
  (ld de long-line)
  (call write-string)

  (jp loop-bars))
