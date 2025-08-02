(import
  (zx-next core)
  (zx-next terminal)
  (zx-next write)
  (zx-next debug))

(define-fragments
  (hello-world (dz "Hello, \x10;\x5;\x11;\x1;world\x10;\x7;\x11;\x08;!\r"))
  (press-space (dz "... Press SPACE ..."))
  (long-line (dz "Hello, this is a very long string which will overflow the terminal by a couple of lines. Really, this is how long it is.")))

(run
  (call terminal-init)

  (ld hl hello-world)
  (call write-string)

  (ld hl hello-world)
  (call write-string)

  (ld hl hello-world)
  (call write-string)

  (ld hl hello-world)
  (call write-string)

  (ld hl #x1f00)
  (call terminal-move-to)

  (ld hl press-space)
  (call write-string)

  (call wait-space)

  (call write-newline)

  (ld hl long-line)
  (call write-string)

  (jp loop-bars))
