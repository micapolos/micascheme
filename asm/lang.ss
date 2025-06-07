(library (asm lang)
  (export asm-put-proc)
  (import (micascheme) (asm fragment))

  (define-syntax (asm-put-proc $syntax $lookup)
    (syntax-case $syntax ()
      ((_ org label)
        (and
          (identifier? #'label)
          (number? (datum org)))
        (org-label->put-proc-syntax
          (lambda ($identifier)
            ($lookup $identifier #'fragment))
          (datum org)
          #'label))))
)
