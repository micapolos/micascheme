(library (asm syntax)
  (export define-asm-syntax asm-bytevector)
  (import (micascheme) (assembler))

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
        (lets
          ($asm
            (fold-left
              (lambda ($asm $item)
                (syntax-case $item ()
                  ((id arg ...)
                    (identifier? #'id)
                    (app
                      (or ($lookup #'id) (syntax-error #'id "undefined asm-syntax"))
                      $asm
                      $item))
                  (id
                    (identifier? #'id)
                    (asm-with-labels $asm
                      (push (asm-labels $asm)
                        #`(id #,(literal->syntax (asm-org $asm))))))))
              (empty-asm)
              (syntaxes item ...)))
          #`(lets
            #,@(reverse (asm-labels $asm))
            #,@(reverse (asm-values $asm))
            (blob->bytevector (blob-append #,@(reverse (asm-blobs $asm)))))))))
)
