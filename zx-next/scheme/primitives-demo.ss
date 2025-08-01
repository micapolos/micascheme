(import
  (zx-next core)
  (zx-next scheme primitives)
  (zx-next terminal)
  (zx-next debug))

(define-fragments
  (hello-string (dz "Hello, world!")))

(run
  (call terminal-init)

  ; init stack
  (init-stack)

  (push-char #\A)
  (push-word #x1234)

  (call println-stack)

  (ld bc #xdede)
  (push-bc)
  (pop-bc)

  (call println-stack)

  (push-byte #x20)
  (push-byte #x40)

  (call println-stack)

  (byte-mul)

  (call println-stack)

  ; (call println)
  ; (call println)
  ; (call println)

  ; (ld e #x00)
  ; (byte-value #x12)
  ; (push-value)

  ; (ld e #x00)
  ; (byte-value #x34)
  ; (push-value)

  ; (byte-add)
  ; (call println)

  ; (ld e #xff)
  ; (byte-value #x12)
  ; (push-value)

  ; (ld e #x00)
  ; (byte-value #x34)
  ; (push-value)

  ; (byte-sub)
  ; (call println)

  (jp loop-bars))
