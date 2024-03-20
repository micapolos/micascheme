(library (zexy z80-bytevector)
  (export
    z80-bytevector)

  (import
    (except (micascheme) and or pop push)
    (zexy assemble)
    (zexy link)
    (zexy vectorize)
    (zexy ops))

  (export
    (import (zexy ops)))


  (define-syntax z80-bytevector
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $op ...)
          (link
            (flatten (map assemble (syntax->list #'($op ...))))
            vectorize)))))
)
