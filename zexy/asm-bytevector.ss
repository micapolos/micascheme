(library (zexy asm-bytevector)
  (export
    define-asm-syntax
    define-asm-syntax-rule
    asm-bytevector)

  (import
    (except (micascheme) and or xor pop push)
    (zexy assemble)
    (zexy link)
    (zexy vectorize)
    (zexy ops))

  (export
    (import (zexy ops)))

  (define-aux-keyword asm)

  (define-syntax-rule (define-asm-syntax $name $transformer)
    (define-property $name asm $transformer))

  (define-syntax-rule (define-asm-syntax-rule ($name $param ...) $body)
    (define-asm-syntax $name
      (lambda ($syntax)
        (syntax-case $syntax ()
          ((_ $param ...) #`$body)))))

  (define-syntax asm-bytevector
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ $op ...)
            (link $lookup
              (flatten
                (map
                  (partial assemble (lambda ($id) ($lookup $id #'asm)))
                  (syntax->list #'($op ...))))
              vectorize))))))
)
