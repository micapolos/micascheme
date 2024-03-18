(import
  (check)
  (zexy link)
  (only (zexy ops) label org db dw))

(define-syntax-case (check-links ($in ...) ($out ...))
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
                  ((dw $expr) #`(list 'dw $expr))))
              $ops))))
      (list (quote $out) ...))))

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
