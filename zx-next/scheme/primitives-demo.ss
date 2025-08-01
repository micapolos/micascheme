(import
  (zx-next core)
  (zx-next scheme primitives)
  (zx-next terminal)
  (zx-next debug))

(run
  (call terminal-init)

  (break)
  (ld e #xff)
  (push-n #x12)
  (break)
  (ld e #x00)
  (push-n #x34)
  (break)
  (add-r-r)
  (break)

  (jp loop-bars))
