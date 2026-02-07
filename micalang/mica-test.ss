(import (micalang base) (micalang mica))

(check-mica
  (let
    (zero?          (native (pi int bool)             (prim zero? a)))
    (+              (native (pi int int int)          (prim + a b)))
    (-              (native (pi int int int)          (prim - a b)))
    (<              (native (pi int int bool)         (prim < a b)))
    (number->string (native (pi int string)           (prim number->string a)))
    (string-length  (native (pi string int)           (prim string-length a)))
    (string-append  (native (pi string string string) (prim string-append a b)))
    (string-append "Hello "
      (number->string
        (string-length "foo"))))
  "Hello 3")

(check-mica 1 1)
(check-mica (zero? 0) #t)
(check-mica (zero? 1) #f)
(check-mica (inc 1) 2)
(check-mica (dec 2) 1)
(check-mica (+ 1 2) 3)
(check-mica (- 3 2) 1)
(check-mica (< 1 2) #t)
(check-mica (< 2 1) #f)

(check-mica
  (let
    (x 10)
    (y 20)
    (+ x y))
  30)

(check-mica
  ((lambda (x int) (inc x)) 2) 3)

(check-mica
  ((lambda (x int) (y int) (+ x y)) 2 3) 5)

(check-mica
  (let
    (a 10)
    (b 20)
    (double (x int) (+ x x))
    (negate (x int) (- 0 x))
    (negate (+ (double a) b)))
  -40)

(check-mica (first-index 16) 0)
(check-mica (last-index 16) 15)

(check-mica
  (let
    (increment (native (pi int int)     (from (micascheme) fx+1/wraparound)))
    (add       (native (pi int int int) (from (micascheme) fx+1/wraparound)))
    (increment (increment 10)))
  12)
