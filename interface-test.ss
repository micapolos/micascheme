(import (scheme) (check) (interface))

(define-interface (property property?)
  (property-get)
  (property-set! x)
  (property-set+! x y))

(define p
  (let ((v 0))
    (property
      (lambda () v)
      (lambda (x) (set! v x))
      (lambda (x y) (set! v (+ x y))))))

(check (= (property-get p) 0))
(property-set! p 20)
(check (= (property-get p) 20))
(property-set+! p 20 30)
(check (= (property-get p) 50))
