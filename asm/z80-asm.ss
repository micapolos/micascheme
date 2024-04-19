(library (asm z80-asm)
  (export z80-op)
  (import (micascheme) (asm z80-gen))

  (define-syntax (define-z80-op $syntax)
    (syntax-case $syntax ()
      ((_ $id) (identifier? #'$id)
        #`(define-syntax ($id $syntax)
          (syntax-case $syntax ()
            ((_ $op #,ellipsis)
              #`(begin
                #,@(map
                  (lambda ($op)
                    (#,(z80-gen) $op))
                  (syntax->list #'($op #,ellipsis))))))))))

  (define-z80-op z80-op)
)
