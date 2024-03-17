(import (check) (micascheme) (labs stacky))

(stacky
  (displayln "Begin...")
  #\newline
  #\!
  #\i
  #\G
  inc
  out
  out
  out
  out
  (displayln "The end!!!"))

(define-syntax assemble
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ $op ...)
        (transform-assemble
          (syntax->list #'($op ...)))))))

(displayln
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
    (db dupa2)))
