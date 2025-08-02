(import
  (zx-next core)
  (zx-next terminal)
  (zx-next bank)
  (zx-next scheme lang))

(define-fragments
  (hello-string (dz "Hello, world!"))
  (hello-symbol (dz "hello-world")))

(run
  (call terminal-init)
  (call banks-init)
  (run-scheme
    (write #t)
    (write #f)
    (write #\a)
    (write (byte #x12))
    (write (word #x1234))
    (write (symbol 0 hello-symbol))
    (write (string 0 hello-string))
    (lets
      (byte #x12)
      (byte #x34)
      (write (byte+ (local 0) (local 4)))))
  (call terminal-wait-space)
  (jp 0))
