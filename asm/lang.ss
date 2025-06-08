(library (asm lang)
  (export asm-blob asm-bytevector)
  (import (micascheme) (asm fragment))

  (define-syntax (asm-blob $syntax $lookup)
    (syntax-case $syntax ()
      ((_ label org)
        (identifier? #'label)
        (program->syntax
          (label->program
            (lambda ($identifier)
              ($lookup $identifier #'fragment))
            (datum org)
            #'label)))))

  (define-rule-syntax (asm-bytevector label org)
    (blob->bytevector (asm-blob label org)))
)
