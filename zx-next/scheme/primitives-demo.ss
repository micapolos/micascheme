(import
  (zx-next core)
  (zx-next scheme primitives)
  (zx-next terminal)
  (zx-next debug))

(define-fragments
  (foo-string (dz "foo"))
  (goodbye-string (dz "Goodbye, world!")))

(run
  (call terminal-init)

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

  (jp loop-bars))
