(import (check) (zexy vectorize) (zexy ops))

(define-syntax check-vectorizes
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ $op ... ($byte ...))
        #`(check
          (equal?
            #,(vectorize (syntax->list #'($op ...)))
            (bytevector $byte ...)))))))

(check-vectorizes ())

(check-vectorizes (db #x12) (#x12))
(check-vectorizes (db (+ #x10 #x02)) (#x12))
(check-vectorizes (db #x12) (db #x34) (#x12 #x34))

(check-vectorizes (dw #x1234) (#x34 #x12))
(check-vectorizes (dw (+ #x1200 #x0034)) (#x34 #x12))
(check-vectorizes (dw #x1234) (dw #x5678) (#x34 #x12 #x78 #x56))
