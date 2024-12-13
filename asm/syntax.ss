(library (asm syntax)
  (export
    define-asm-syntax asm-bytevector
    org eq db)
  (import (micascheme) (asm))

  (define-aux-keywords org eq db)

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
        (asm->syntax
          (fold-left
            (lambda ($asm $item)
              (syntax-case $item (eq org db)
                (id
                  (identifier? #'id)
                  (asm+label $asm #'id))
                ((org expr)
                  (asm-with-org $asm (datum org)))
                ((eq id expr)
                  (identifier? #'id)
                  (asm+value $asm #'id #'expr))
                ((db expr ...)
                  (fluent $asm
                    (asm+blob #'(u8-blob expr ...))
                    (asm+org (length (syntaxes expr ...)))))
                ((id arg ...)
                  (identifier? #'id)
                  (switch ($lookup #'id)
                    ((procedure? $procedure) ($procedure $asm $item))
                    ((else _) (syntax-error #'id "undefined asm-syntax"))))))
            (empty-asm)
            (syntaxes item ...))))))
)
