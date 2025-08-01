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
  (ld e #xff)
  (push-value)

  (ld e #x00)
  (char-value #\A)
  (push-value)

  (ld e #x00)
  (word-value #x1234)
  (push-value)

  (ld e #x00)
  (byte-value #x12)
  (push-value)

  (dup-value 0)

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
