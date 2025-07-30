(import
  (zx-next core)
  (zx-next terminal)
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
  (ld de #x0000)
  (ld hl (fxior hello-world-symbol #x01))  ; why value-tag-symbol does not work?
  (call scheme-write)
  (call loop-bars))

