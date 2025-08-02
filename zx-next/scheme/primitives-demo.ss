(import
  (zx-next core)
  (zx-next write)
  (zx-next scheme primitives)
  (zx-next terminal)
  (zx-next bank)
  (zx-next debug))

(define-fragments
  (hello-string (dz "=== Hello, Scheme!!! ==="))
  (goodbye-string (dz "=== Goodbye, Scheme! ==="))
  (foo-string (dz "foo")))

(run
  (call terminal-init)
  (call banks-init)

  (ld hl hello-string)
  (call writeln-string)

  (run-scheme
    (push-null)
    (push-true)
    (push-false)
    (push-symbol foo-string)
    (push-string foo-string)
    (push-char #\A)
    (push-word #x1234)

    (call println-stack)
    (call println)

    (ld bc #xdede)
    (push-bc)
    (call println-stack)

    (pop-bc)
    (call println-stack)

    (push-byte #x20)
    (push-byte #x40)

    (call println-stack)

    (byte-mul)

    (call println-stack)

    (pop-value)
    (pop-value)
    (call println-stack))

  (ld hl goodbye-string)
  (call writeln-string)

  (jp loop-bars))
