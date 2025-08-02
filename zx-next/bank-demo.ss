(import
  (zx-next core)
  (zx-next terminal)
  (zx-next bank)
  (zx-next debug))

(run
  (call terminal-init)

  (call write-banks)
  (call terminal-wait-space)

  (loop
    (ld a #x12)
    (call bank-alloc)
    (while nc))
  (call write-banks)
  (call terminal-wait-space)

  (ld a #x34)
  (call bank-free)
  (call write-banks)
  (call terminal-wait-space)

  (call loop-bars))
