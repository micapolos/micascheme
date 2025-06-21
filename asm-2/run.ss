(library (asm-2 run)
  (export run-asm)
  (import (rename (micascheme) (let* %let*) (define %define)) (cspect) (nex) (asm-2 lang))

  (define-rule-syntax (run-asm body ...)
    (lets
      ($path "/tmp/main.nex")
      (run
        (call-with-port (open-file-output-port $path (file-options no-fail))
          (lambda ($port)
            (lets
              ($bytevector
                (asm
                  (binary->bytevector
                    (asm-binary
                      (org #xc000)
                      body ...))))
              (run (pretty-print $bytevector))
              (put-blob $port (nex-blob (bytevector->blob $bytevector))))))
        (cspect $path))))
)
