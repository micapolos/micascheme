(library (asm syntax)
  (export
    define-asm-syntax
    asm-bytevector)
  (import (micascheme) (asm))

  (define-rules-syntax
    ((define-asm-syntax id proc)
      (identifier? #'id)
      (define-syntax id (make-compile-time-value proc)))
    ((define-asm-syntax (id $asm $syntax) body)
      (for-all identifier? (syntaxes id $asm $syntax))
      (define-asm-syntax id
        (lambda ($asm $syntax) body)))
    ((define-asm-syntax (id param ...) ($asm) body)
      (for-all identifier? (syntaxes id param ...))
      (define-asm-syntax (id $asm $syntax)
        (syntax-case $syntax ()
          ((id param ...) body)))))

  (define-syntax (asm-bytevector $syntax $lookup)
    (syntax-case $syntax ()
      ((id item ...)
        (asm->bytevector-syntax
          (fold-left
            (lambda ($asm $item)
              (syntax-case $item (eq org db)
                ((id arg ...)
                  (and (identifier? #'id) ($lookup #'id))
                  (($lookup #'id) $asm $item))
                (other
                  (asm+syntax $asm #'other))))
            (empty-asm)
            (syntaxes item ...))))))
)
