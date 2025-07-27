(library (asm run)
  (export run)
  (import
    (rename (asm base) (run %run))
    (only (asm lang) asm org)
    (asm assembled)
    (cspect)
    (nex))
  (export
    (import
      (asm lang)
      (asm z80)))

  (define-rule-syntax (run body ...)
    (lets
      ($path "/tmp/main.nex")
      ((assembled $start $binary) (asm (org #xc000) body ...))
      ($bytevector (binary->bytevector $binary))
      (run
        (parameterize ((print-radix 16))
          (pretty-print $bytevector)))
      ($nex-blob (nex-blob (bytevector->blob $bytevector) $start))
      (%run
        (call-with-port
          (open-file-output-port $path (file-options no-fail))
          (lambda ($port) (put-blob $port $nex-blob)))
        (cspect $path))))
)
