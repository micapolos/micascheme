(import
  (check)
  (zexy link)
  (only (zexy ops) label org db dw ds align))

(define-syntax (check-links $syntax)
  (syntax-case $syntax ()
    ((_ ($in ...) ($out ...))
      #`(check
        (equal?
          #,(link
            (syntax->list #'($in ...))
            (lambda ($ops)
              #`(list
                #,@(map
                  (lambda ($op)
                    (syntax-case $op (db dw)
                      ((db $expr) #`(list 'db $expr))
                      ((dw $expr) #`(list 'dw $expr))
                      ((ds $expr) #`(list 'ds $expr))))
                  $ops))))
          (list (quote $out) ...))))))

(check-links () ())

(check-links
  ((db #x12))
  ((db #x12)))

(check-links
  ((db (+ #x10 #x02)))
  ((db #x12)))

(check-links
  ((dw #x1234))
  ((dw #x1234)))

(check-links
  ((dw (+ #x1200 #x0034)))
  ((dw #x1234)))

(check-links
  ((ds 3))
  ((ds 3)))

(check-links
  (
    (db #x12)
    (label foo)
    (dw foo))
  (
    (db #x12)
    (dw #x0001)))

(check-links
  (
    (db #x12)
    (label foo)
    (dw (+ foo #x300)))
  (
    (db #x12)
    (dw #x0301)))

(check-links
  (
    (org #x2000)
    (db #x12)
    (label foo)
    (dw foo))
  (
    (db #x12)
    (dw #x2001)))

(check-links
  (
    (align 4)
    (db #x11))
  (
    (db #x11)))

(check-links
  (
    (db #x11)
    (align 4)
    (db #x55))
  (
    (db #x11)
    (ds 3)
    (db #x55)))

(check-links
  (
    (db #x11)
    (db #x22)
    (db #x33)
    (db #x44)
    (align 4)
    (db #x55))
  (
    (db #x11)
    (db #x22)
    (db #x33)
    (db #x44)
    (db #x55)))
