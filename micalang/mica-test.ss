(import (micalang base) (micalang mica))

(check-mica
  (let
    (val number->string
      (native
        (pi number string)
        %%number->string))
    (val string-length
      (native
        (pi string number)
        %%string-length))
    (val string-append
      (native
        (pi string string string)
        (%%lambda (a) (%%lambda (b) (%%string-append a b)))))
    (string-append "Hello "
      (number->string
        (string-length "foo"))))
  "Hello 3")

(check-mica 1 1)
(check-mica (zero? 0) #t)
(check-mica (zero? 1) #f)
(check-mica (+ 1 2) 3)
(check-mica (- 3 2) 1)
(check-mica (< 1 2) #t)
(check-mica (< 2 1) #f)

(check-mica
  (let
    (val x 10)
    (val y 20)
    (+ x y))
  30)

(check-mica
  ((lambda (val x number) (val y number) (+ x y)) 2 3) 5)

(check-mica
  (let
    (val a 10)
    (val b 20)
    (val double (val x number) (+ x x))
    (val negate (val x number) (- 0 x))
    (negate (+ (double a) b)))
  -40)

(check-mica (if (zero? 0) "zero" "not-zero") "zero")
(check-mica (if (zero? 1) "zero" "not-zero") "not-zero")

(check-mica fx #f)
(check-mica (fx 12) 12)

;(check-mica ((lambda (x fx) x) (fx 12)) 12)

(check-mica
  (let
    (val max-fx (native fx (%%most-positive-fixnum)))
    (val min-fx (native fx (%%most-negative-fixnum)))
    (val fx+ (val a fx) (val b fx) (native fx (%%fx+/wraparound a b)))
    (val fx+1 (val a fx) (fx+ a (fx 1)))
    (fx+ max-fx (fx 1)))
  (most-negative-fixnum))
