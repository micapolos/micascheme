(import (micalang base) (micalang mica))

(check-mica
  (let
    (number->string (native (pi number string) (curry %%number->string (n number))))
    (string-length (native (pi string number) (curry %%string-length (s string))))
    (string-append (native (pi string string string) (curry %%string-append (a string) (b string))))
    (string-append "Hello " (number->string (string-length "foo"))))
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
    (x 10)
    (y 20)
    (+ x y))
  30)

(check-mica
  ((lambda (x number) (y number) (+ x y)) 2 3) 5)

(check-mica
  (let
    (a 10)
    (b 20)
    (double (x number) (+ x x))
    (negate (x number) (- 0 x))
    (negate (+ (double a) b)))
  -40)

(check-mica (if (zero? 0) "zero" "not-zero") "zero")
(check-mica (if (zero? 1) "zero" "not-zero") "not-zero")

(check-mica
  (let
    (fx (macro (c t)
      (%%syntax-case t ()
        (id (%%symbol? (%%datum id))
          '(native type (constant fx)))
        ((_ n)
          (%%if (%%fixnum? (%%datum n))
            `(native fx (tagged (constant fx) ,(%%datum n)))
            (%%syntax-error #'n "not fixnum"))))))
    (fx-10 (fx 10))
    (fx-id (lambda (x fx) x))
    (fx-id fx-10))
  10)

; === dependent identity

(check-mica
  (let
    (id (lambda (t type) (x t) x))
    (id number 12))
  12)

