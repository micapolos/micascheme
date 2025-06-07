(library (asm lang)
  (export asm-put-proc)
  (import (micascheme) (asm fragment))

  (define-syntax (asm-put-proc $syntax $lookup)
    (syntax-case $syntax ()
      ((_ label)
        (identifier? #'label)
        (org-label->put-proc-syntax
          (lambda ($identifier)
            ($lookup $identifier #'fragment))
          #'label))))
)
