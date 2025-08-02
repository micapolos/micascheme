(import
  (zx-next core)
  (zx-next terminal)
  (zx-next bank)
  (zx-next scheme lang))

(run
  (call terminal-init)
  (call banks-init)
  (call terminal-wait-space)
  (jp 0))
