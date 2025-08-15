(library (zx-next compiler indexed-expr)
  (export)
  (import (micascheme))

  (define-keywords arg local)

  (define (indexed->expr $syntax)
    (syntax-case $syntax ()
      (((arg-offset arg-offset* ...) locals 1 (arg 0))
        #`(size #t (ld a (+ ix offset))))))
)
