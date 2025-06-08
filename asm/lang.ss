(library (asm lang)
  (export
    define-asm
    asm-blob
    asm-bytevector
    start)
  (import (micascheme) (asm fragment) (asm program) (asm expression) (asm block) (nex) (cspect))

  ; TODO: Replace with empty fragment
  (meta define main-fragment
    (make-thread-parameter
      (fragment-with ()
        (u8-block
          #xf3 ; DI
          #x3e ; LD A, 0
          #b00000010
          #xd3 ; OUT ($fe), A
          #xfe
          #x3e ; LD A, $ff
          #b00010101
          #xd3 ; OUT ($fe), A
          #xfe
          #xc3 ; JMP $c001
          #x01
          #xc0))))

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
                    #,(program->syntax
                      (fragment->program $lookup #xc000 (main-fragment)))))))
            (cspect $path))))))
)
