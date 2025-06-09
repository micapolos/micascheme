(library (asm lang)
  (export
    define-asm
    asm-blob
    asm-bytevector
    start
    (rename
      (%label label)
      (%db db)
      (%dw dw))
    di ld out jp a)
  (import (micascheme) (asm fragment) (asm program) (asm expression) (asm block) (asm frame) (nex) (cspect))

  (meta define main-frame
    (make-thread-parameter (empty-frame)))

  (define-rule-syntax (define-ops (op target) ...)
    (begin
      (define-syntax (op $syntax $lookup)
        (syntax-case $syntax ()
          ((_ arg (... ...))
            (run
              (main-frame (frame+syntax (main-frame) #'(target arg (... ...))))
              #`(void))))) ...))

  (define-ops
    (%label label)
    (%db db)
    (%dw dw))

  (define-keywords a)

  (define-rule-syntax (di) (%db #xf3))
  (define-rule-syntax (ld a n) (%db #x3e n))
  (define-rule-syntax (out (n) a) (%db #xd3 n))
  (define-rule-syntax (jp nn) (begin (%db #xc3) (%dw nn)))

  (define-rule-syntax (define-asm label fragment)
    (define-syntax label (make-compile-time-value fragment)))

  (define-syntax (asm-blob $syntax $lookup)
    (syntax-case $syntax ()
      ((_ label org)
        (identifier? #'label)
        (program->syntax
          (datum org)
          (label->program
            $lookup
            #'label)))))

  (define-rule-syntax (asm-bytevector label org)
    (blob->bytevector (asm-blob label org)))

  (define-syntax (start $syntax $lookup)
    (syntax-case $syntax ()
      ((_)
        #`(lets
          ($path "/tmp/main.nex")
          (run
            (call-with-port (open-file-output-port $path (file-options no-fail))
              (lambda ($port)
                (put-blob $port
                  (nex-blob
                    #,(lets
                      ($syntax (frame->syntax #xc000 (main-frame)))
                      (run (pretty-print (syntax->datum $syntax)))
                      #`(lets
                        ($blob #,$syntax)
                        (run (pretty-print (blob->bytevector $blob)))
                        $blob))))))
            (cspect $path))))))
)
