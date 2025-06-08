(library (asm lang)
  (export
    define-asm
    asm-blob
    asm-bytevector)
  (import (micascheme) (asm fragment))

  (define-rule-syntax (define-asm label fragment)
    (define-syntax label (make-compile-time-value fragment)))

  (define-syntax (asm-blob $syntax $lookup)
    (syntax-case $syntax ()
      ((_ label org)
        (identifier? #'label)
        (program->syntax
          (label->program
            $lookup
            (datum org)
            #'label)))))

  (define-rule-syntax (asm-bytevector label org)
    (blob->bytevector (asm-blob label org)))
)
