(import (scheme) (check) (int) (syntax))

(define-rule-syntax (check-expands in out)
  (check-equal?
    (expand 'in (environment '(int) '(scheme)))
    (expand 'out (environment '(int) '(scheme)))))

(check-expands (int-mask 0) #b0)
(check-expands (int-mask 1) #b1)
(check-expands (int-mask 4) #b1111)

(check-expands (int 4 #x1234) (bitwise-and #x1234 (int-mask 4)))

(check-expands (int+ 4 #x3f #x56) (int 4 (+ #x3f #x56)))
(check-expands (int- 4 #x3f #x56) (int 4 (- #x3f #x56)))
(check-expands (int* 4 #x3f #x56) (int 4 (* #x3f #x56)))

(check-expands (int+1 4 #x3f) (int+ 4 #x3f 1))
(check-expands (int-1 4 #x3f) (int- 4 #x3f 1))
