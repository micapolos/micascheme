(import (scheme) (check) (data) (syntax))

; === data ===

(data (point x y))
(data (point2 x y))
(check (equal? (point 1 2) (point 1 2)))
(check (equal? (point? (point 1 2)) #t))
(check (equal? (point-x (point 1 2)) 1))
(check (equal? (point-y (point 1 2)) 2))

(check (equal? (point-with-x (point 1 2) 3) (point 3 2)))
(check (equal? (point-with-y (point 1 2) 3) (point 1 3)))

(check (= (equal-hash (point 1 2)) (equal-hash (point 1 2))))
(check (not (= (equal-hash (point 1 2)) (equal-hash (point 1 3)))))

; (check
;   (equal?
;     (lets
;       ((point x y) (point "a" "b"))
;       (string-append x y))
;     "ab"))

; (check
;   (equal?
;     (lets
;       ((point x (point y z)) (point "a" (point "b" "c")))
;       (string-append x y z))
;     "abc"))

; === data vararg ===

(data (vpoint x y . v))
(data (vpoint2 x y . v))
(check (equal? (vpoint 1 2) (vpoint 1 2)))
(check (equal? (vpoint 1 2 3) (vpoint 1 2 3)))
(check (equal? (vpoint 1 2 3 4) (vpoint 1 2 3 4)))
(check (equal? (vpoint 1 2 3 4) (make-vpoint 1 2 (list 3 4))))
(check (equal? (vpoint? (vpoint 1 2 3 4)) #t))
(check (equal? (vpoint-x (vpoint 1 2 3 4)) 1))
(check (equal? (vpoint-y (vpoint 1 2 3 4)) 2))
(check (equal? (vpoint-v (vpoint 1 2)) (list)))
(check (equal? (vpoint-v (vpoint 1 2 3 4)) (list 3 4)))

(check (equal? (vpoint-with-x (vpoint 1 2 3 4) 5) (vpoint 5 2 3 4)))
(check (equal? (vpoint-with-y (vpoint 1 2 3 4) 5) (vpoint 1 5 3 4)))
(check (equal? (vpoint-with-v (vpoint 1 2 3 4) (list 5 6)) (vpoint 1 2 5 6)))

(check (= (equal-hash (vpoint 1 2 3 4)) (equal-hash (vpoint 1 2 3 4))))
(check (not (= (equal-hash (vpoint 1 2 3 4)) (equal-hash (vpoint 1 2 3 5)))))

; (check
;   (equal?
;     (lets
;       ((vpoint x y . z) (vpoint "a" "b" "c" "d"))
;       (string-append x y (apply string-append z)))
;     "abcd"))

; (check
;   (equal?
;     (lets
;       ((vpoint x (vpoint y z . t1) . t2) (vpoint "a" (vpoint "b" "c" "d" "e") "f" "g"))
;       (string-append x y z (apply string-append t1) (apply string-append t2)))
;     "abcdefg"))

; === data unit ===

(data foo)
(check (record? foo))
(check (equal? foo foo))
(check (foo? foo))

(data bar)
(check (record? bar))
(check (equal? bar bar))
(check (bar? bar))

(check (not (equal? foo bar)))
(check (not (foo? bar)))
(check (not (bar? foo)))

; === data*

(data*
  (circle radius)
  (square size)
  (rectangle width height))

; === enum ===

(enum (foolik number string))

(check (equal? (foolik? (foolik 123)) #t))
(check (equal? (foolik? (foolik "foo")) #t))

(check (equal? (foolik 123) (foolik 123)))
(check (not (equal? (foolik 123) (foolik 124))))

(check (equal? (equal-hash (foolik 123)) (equal-hash (foolik 123))))
(check (not (equal? (equal-hash (foolik 123)) (equal-hash (foolik 124)))))

(check (equal? (foolik-body (foolik "foo")) "foo"))

(check
  (equal?
    (foolik-switch (foolik "foo")
      ((string? $string) (string-append $string "!"))
      ((number? $number) (number->string $number)))
    "foo!"))

(check
  (equal?
    (foolik-switch (foolik 128)
      ((string? $string) (string-append $string "!"))
      ((number? $number) (number->string $number)))
    "128"))
