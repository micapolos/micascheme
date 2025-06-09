(library (asm z80n)
  (export
    label db dw
    di ld out jp a loop
    run)
  (import
    (rename (micascheme) (run %run))
    (asm lang)
    (nex)
    (cspect)
    (rename (asm fragment) (db %db) (dw %dw))
    (rename (asm frame) (label %label)))

  (define-ops
    (label %label)
    (db %db)
    (dw %dw))

  (define-keywords a)

  (define-rule-syntax (di) (db #xf3))
  (define-rule-syntax (ld a n) (db #x3e n))
  (define-rule-syntax (out (n) a) (db #xd3 n))
  (define-rule-syntax (jp nn) (begin (db #xc3) (dw nn)))

  (define-case-syntax (loop body ...)
    (lets
      ($tmp (generate-temporary #'loop))
      #`(begin
        (label #,$tmp)
        body ...
        (jp #,$tmp))))

  (define-syntax (run $syntax $lookup)
    (syntax-case $syntax ()
      ((_)
        #`(lets
          ($path "/tmp/main.nex")
          (%run
            (call-with-port (open-file-output-port $path (file-options no-fail))
              (lambda ($port)
                (put-blob $port
                  (nex-blob
                    (lets
                      ($blob (main-blob))
                      (%run (pretty-print (blob->bytevector $blob)))
                      $blob)))))
            (cspect $path))))))
)
