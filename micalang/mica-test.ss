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
    (any-fx (native type (constant any-fx)))
    (fx (macro (c t)
      (%%syntax-case t ()
        ((_ n)
          (%%if (%%fixnum? (%%datum n))
            `(native any-fx (tagged (constant any-fx) ,(%%datum n)))
            (%%syntax-error #'n "not fixnum"))))))
    (fx-10 (fx 10))
    (fx-id (lambda (x any-fx) x))
    (fx+ (native (pi any-fx any-fx any-fx) (curry %%fx+ (a any-fx) (b any-fx))))
    (fx+1 (a any-fx) (fx+ a (fx 1)))
    (fx+1 (fx+ (fx 10) (fx 20))))
  31)

; === dependent identity

(check-mica
  (let
    (id (lambda (t type) (x t) x))
    (id number 12))
  12)

; === a 2D vector of anything

(check-mica
  (let
    (any-vec2
      (lambda (t type)
        (
          (native
            (pi symbol type type)
            (curry %%list (s symbol) (t type)))
          'any-vec2
          t)))
    (vec2
      (lambda (element type)
        (native
          (pi element element (any-vec2 element))
          (curry %%cons (a element) (b element)))))
    (vec2 (any-vec2 number) (vec2 number 10 20) (vec2 number 10 20)))
  `((10 . 20) . (10 . 20)))
