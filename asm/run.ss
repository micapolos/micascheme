(library (asm run)
  (export run)
  (import
    (rename (micascheme) (let* %let*) (define %define) (run %run))
    (cspect)
    (nex)
    (asm lang)
    (asm asm))

  (define-rule-syntax (run body ...)
    (lets
      ($path "/tmp/main.nex")
      (%run
        (call-with-port (open-file-output-port $path (file-options no-fail))
          (lambda ($port)
            (lets
              ($bytevector (asm-bytevector (org #xc000) body ...))
              (run (pretty-print $bytevector))
              (put-blob $port (nex-blob (bytevector->blob $bytevector))))))
        (cspect $path))))
)
