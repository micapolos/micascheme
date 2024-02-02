(library (zexy)
  (export compile-zexy)
  (import
    (micascheme)
    (zexy asm))

  (define-syntax-rule (compile-zexy $filename $arg ...)
    (lets
      ($port (open-file-output-port $filename (file-options no-fail) (buffer-mode none) #f))
      (run
        (put-bytevector $port
          (asm-bytevector
            (asm-ops (empty-asm) (list #'$arg ...)))))
        (close-output-port $port)))
)
