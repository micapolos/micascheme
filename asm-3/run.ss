(library (asm-3 run)
  (export run)
  (import
    (rename (asm-3 base) (run %run))
    (only (asm-3 lang) asm org)
    (asm-3 assembled)
    (cspect)
    (nex))
  (export
    (import
      (asm-3 lang)
      (asm-3 z80)))

  (define-rule-syntax (run body ...)
    (lets
      ($path "/tmp/main.nex")
      ((assembled $start $binary) (asm (org #xc000) body ...))
      ($bytevector (binary->bytevector $binary))
      (run (pretty-print $bytevector))
      ($nex-blob (nex-blob (bytevector->blob $bytevector) $start))
      (%run
        (call-with-port
          (open-file-output-port $path (file-options no-fail))
          (lambda ($port) (put-blob $port $nex-blob)))
        (cspect $path))))
)
