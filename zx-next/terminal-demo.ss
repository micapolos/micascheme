(import
  (zx-next core)
  (zx-next terminal)
  (zx-next debug))

(run
  (call terminal-init)
  (jp loop-bars))
