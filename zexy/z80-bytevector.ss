(library (zexy z80-bytevector)
  (export
    define-z80-syntax
    define-z80-syntax-rule
    z80-bytevector)

  (import
    (except (micascheme) and or pop push)
    (zexy assemble)
    (zexy link)
    (zexy vectorize)
    (zexy ops))

  (export
    (import (zexy ops)))

  (define-aux-keyword z80)

  (define-syntax-rule (define-z80-syntax $name $transformer)
    (define-property $name z80 $transformer))

  (define-syntax-rule (define-z80-syntax-rule ($name $param ...) $body)
    (define-z80-syntax $name
      (lambda ($syntax)
        (syntax-case $syntax ()
          ((_ $param ...) #`$body)))))

  (define-syntax z80-bytevector
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ $op ...)
            (link
              (flatten
                (map
                  (partial assemble (lambda ($id) ($lookup $id #'z80)))
                  (syntax->list #'($op ...))))
              vectorize))))))
)
