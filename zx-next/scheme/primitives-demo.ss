(import
  (zx-next core)
  (zx-next write)
  (zx-next scheme primitives)
  (zx-next terminal)
  (zx-next bank)
  (zx-next debug))

(define-fragments
  (foo-string (dz "foo"))
  (custom-error-string (dz "I don't want to do it anymore.")))

(run
  (call terminal-init)
  (call banks-init)

  (writeln "=== Hello, Scheme!!! ===")
  (writeln)

  (run-scheme
    (push-null)
    (push-true)
    (push-false)
    (push-symbol 0 foo-string)
    (push-string 0 foo-string)
    (push-char #\A)
    (push-word #x1234)

    (call write-stack)
    (call println)

    (ld bc #xdede)
    (push-bc)
    (call write-stack)

    (pop-bc)
    (call write-stack)

    (push-byte #x20)
    (push-byte #x40)

    (call write-stack)

    (byte-mul)

    (call write-stack)

    (pop-value)
    (pop-value)
    (call write-stack))

  (writeln)
  (writeln "=== Goodbye, Scheme!!! ===")

  (jp loop-bars))
