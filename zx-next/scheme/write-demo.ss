(import
  (zx-next core)
  (zx-next terminal)
  (zx-next scheme write)
  (zx-next debug))

(define-fragments
  (hello-string (dz "Hello, world!\r")))

(run
  (call terminal-init)
  (ld de #x0000)
  (ld hl hello-string)
  (call scheme-write)
  (call loop-bars))

