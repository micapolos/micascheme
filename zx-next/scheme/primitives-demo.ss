(import
  (zx-next core)
  (zx-next scheme primitives)
  (zx-next terminal)
  (zx-next debug))

(run
  (call terminal-init)

  (ld e #xff)
  (byte-value #x12)
  (push-value)

  (ld e #x00)
  (word-value #x1234)
  (push-value)

  (ld e #x00)
  (char-value #\A)
  (push-value)

  (ld e #x00)
  (char-value #\A)
  (push-value)

  (call println)
  (call println)
  (call println)

  (jp loop-bars))
