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
  (dump hello-string #x30)
  (dump goodbye-string #x30)
  (dump foo-string #x30)
  (writeln)
  (writeln "=== Hello, Scheme!!! ===")
  (writeln)

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

  (writeln)
  (writeln "=== Goodbye, Scheme!!! ===")

  (jp loop-bars))
