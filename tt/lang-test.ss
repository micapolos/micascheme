(import
  (tt lang)
  (prefix (scheme) %)
  (prefix (check) %)
  (prefix (tt hoas) %)
  (prefix (tt primitive) %))

(define-type boolean)
(define-type string)
(define-type (list element))
(define-type (pair car cdr))

(%check
  (%equal?
    (tt (pi 10 20 30))
    (%arrow* (%native 10) (%native 20) (%native 30))))

(define n (lambda (x type) x))
