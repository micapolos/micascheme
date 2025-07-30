(library (asm make-dot)
  (export make-dot)
  (import
    (asm base)
    (only (asm lang) align asm org with-labels begin)
    (only (asm z80) jp)
    (asm assembled))

  (define-rule-syntax (make-dot name body ...)
    (lets
      ($path name)
      ((assembled $start $binary)
        (asm (org #x2000)
          (with-labels (main)
            ; Force this to be at the beginning.
            (begin
              (align #x2000)
              (jp main))
            main body ...)))
      (run
        (parameterize ((print-radix 16))
          (pretty-print (binary->datum $binary))
          (pretty-print `(start ,$start))
          (pretty-print `(path ,$path))))
      (run
        (call-with-port
          (open-file-output-port $path (file-options no-fail))
          (lambda ($port) (put-binary $port $binary))))))
)
