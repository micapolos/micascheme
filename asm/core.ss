(library (asm core)
  (export
    define-asm
    asm-blob
    asm-bytevector
    main-blob
    define-ops)
  (import (micascheme) (asm fragment) (asm program) (asm expression) (asm block) (asm frame) (nex) (cspect))

  (meta define main-frame
    (make-thread-parameter (empty-frame)))

  (define-syntax (main-blob $syntax)
    (syntax-case $syntax ()
      ((_)
        (frame->syntax #xc000 (main-frame)))))

  (define-rule-syntax (define-ops (op target) ...)
    (begin
      (define-syntax (op $syntax $lookup)
        (syntax-case $syntax ()
          ((_ arg (... ...))
            (run
              (main-frame (frame+syntax (main-frame) #'(target arg (... ...))))
              #`(void))))) ...))

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
)
