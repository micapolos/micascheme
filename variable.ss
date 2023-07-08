(library (variable)
  (export variable variable? variable-index v0 v1 v2)
  (import (micascheme))

  (data (variable index))

  (define-syntax v0
    (lambda (stx)
      (syntax-case stx ()
        (_ #`(variable 0)))))

  (define-syntax v1
    (lambda (stx)
      (syntax-case stx ()
        (_ #`(variable 1)))))

  (define-syntax v2
    (lambda (stx)
      (syntax-case stx ()
        (_ #`(variable 2)))))
)