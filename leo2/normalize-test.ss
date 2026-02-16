(import (leo2 base) (leo2 term) (leo2 normalize))

(define foo (native 0 "foo" (stack)))
(define bar (native 0 "bar" (stack)))

(define (%string-length $lhs)
  (if (native? $lhs)
    (native 0 (string-length (native-value $lhs)) (stack))
    (native 0 string-length (stack $lhs))))

(define (%string-append $lhs $rhs)
  (if (and (native? $lhs) (native? $rhs))
    (native 0 (string-append (native-value $lhs) (native-value $rhs)) (stack))
    (native 0 string-append (stack $lhs $rhs))))

(check
  (equal?
    (normalize (stack) (native 0 10 (list)))
    (native 0 10 (stack))))

(check
  (equal?
    (normalize (stack) (application (native 1 %string-length (stack)) foo))
    (native 0 3 (stack))))

(check
  (equal?
    (normalize (stack) (application (native 2 %string-append (stack)) foo))
    (native 1 %string-append (stack foo))))

(check
  (equal?
    (normalize (stack) (application (application (native 2 %string-append (stack)) foo) bar))
    (native 0 "foobar" (stack))))
