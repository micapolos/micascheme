(import
  (zx-next core)
  (zx-next debug)
  (zx-next write)
  (zx-next terminal)
  (zx-next scheme value)
  (zx-next scheme write)
  (zx-next scheme stack))

(run
  (call terminal-init)
  (call stack-init)

  (value-word #x1234)
  (stack-preserve
    (value-preserve
      (call scheme-write)
      (call write-newline)))
  (call stack-push)

  (value-word #x5678)
  (stack-preserve
    (value-preserve
      (call scheme-write)
      (call write-newline)))

  (call stack-pop)
  (stack-preserve
    (call scheme-write)
    (call write-newline))

  (call wait-space)

  (call loop-bars))
