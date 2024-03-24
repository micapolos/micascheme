(import (check) (micascheme) (labs stacky))

(define-syntax (assemble $syntax)
  (syntax-case $syntax ()
    ((_ $op ...)
      (transform-assemble
        (syntax->list #'($op ...))))))

(check
  (equal?
    (assemble
      (dz #xff)
      (call (+ dupa1 123))
      (ret)
      (nop)
      (db 12)
      (label dupa1)
      (dw #x0102)
      (label dupa2)
      (dw (+ 12 345))
      (begin
        (call dupa2)
        (ret))
      (db 0)
      (db dupa1)
      (db dupa2))
    #vu8(255 0 205 131 0 201 0 12 2 1 101 1 205 10 0 201 0 8 10)))
