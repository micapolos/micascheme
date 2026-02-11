(import
  (micalang base)
  (micalang term)
  (micalang compiler))

; === type

(check-compiles type (compiled type type type))

; === native

(check-compiles
  (native boolean foo)
  (compiled boolean boolean foo))

(check-compiles
  (let
    (zero? (native (pi number boolean) (curry %%zero? (x number))))
    (zero? 1))
  (compiled boolean boolean
    (let
      (zero? (curry %%zero? (x number)))
      (app zero? (native 1)))))

(check-compiles
  (let
    (add (native (pi number number number) (curry %%+ (a number) (b number))))
    (increment (a number) (+ a 1))
    (double (a number) (b number) (add a b))
    (double (increment 1)))
  (compiled
    (pi (b number) number)
    (pi (b number) number)
    (let (add (curry %%+ (a number) (b number)))
      (let (increment (lambda (a number) (app (app + a) (native 1))))
        (let (double (lambda (a number) (lambda (b number) (app (app add a) b))))
          (app double (app increment (native 1))))))))

(check-compiles
  (native (pi number number number) (native (%lambda (x) (%lambda (y) (%+ x y)))))
  (compiled
    (pi number number number)
    (pi number (pi number number))
    (native (%lambda (x) (%lambda (y) (%+ x y))))))

; === natives

(check-compiles #f (compiled boolean boolean (native #f)))
(check-compiles #t (compiled boolean boolean (native #t)))
(check-compiles 123 (compiled number number (native 123)))
(check-compiles 3.14 (compiled number number (native 3.14)))
(check-compiles 'foo (compiled symbol symbol (native 'foo)))
(check-compiles #\a (compiled char char (native #\a)))
(check-compiles "foo" (compiled string string (native "foo")))

; === globals

(check-compiles boolean (compiled type type boolean))
(check-compiles number (compiled type type number))
(check-compiles string (compiled type type string))

(check-compiles
  zero?
  (compiled
    (pi number boolean)
    (pi number boolean)
    zero?))

(check-compiles
  <
  (compiled
    (pi number number boolean)
    (pi number number boolean)
    <))

; === the type

(check-compiles
  (the (pi number boolean))
  (compiled
    (pi number boolean)
    (pi number boolean)
    zero?))

(check-compiles
  (the (pi number boolean) zero?)
  (compiled
    (pi number boolean)
    (pi number boolean)
    zero?))

(check-compiles
  (the (pi number number boolean))
  (compiled
    (pi number number boolean)
    (pi number number boolean)
    <))

(check-compiles
  (the (pi number number boolean) <)
  (compiled
    (pi number number boolean)
    (pi number number boolean)
    <))

(check-compiles
  (the (pi number number number) +)
  (compiled
    (pi number number number)
    (pi number number number)
    +))

; conflict between - and +
(check-compile-raises
  (the (pi number number number)))

; === application

(check-compiles
  (zero? 10)
  (compiled
    boolean
    boolean
    (app zero? (native 10))))

(check-compiles
  ((+ 10) 20)
  (compiled
    number
    number
    (app (app + (native 10)) (native 20))))

(check-compiles
  (+ 10 20)
  (compiled
    number
    number
    (app (app + (native 10)) (native 20))))

(check-compile-raises (zero? #t))
(check-compile-raises (+ 1 #t))
(check-compile-raises (+ #t 2))

; === let

(check-compiles
  (let 10)
  (compiled number number (native 10)))

(check-compiles
  (let (x 10) (zero? x))
  (compiled boolean boolean (let (x (native 10)) (app zero? x))))

(check-compiles
  (let (x 10) (y 20) (< x y))
  (compiled
    boolean
    boolean
    (let (x (native 10))
      (let (y (native 20))
        (app (app < x) y)))))

(check-compiles
  (let
    (zwiększ (x number) (+ x 1))
    (zwiększ 10))
  (compiled
    number
    number
    (let
      (zwiększ (lambda (x number) (app (app + x) (native 1))))
      (app zwiększ (native 10)))))

; === lambda

(check-compiles
  (lambda 12)
  (compiled number number (native 12)))

(check-compiles
  (lambda (i number) i)
  (compiled
    (pi (i number) number)
    (pi (i number) number)
    (lambda (i number) i)))

(check-compiles
  (lambda (i number) (j number) (+ i j))
  (compiled
    (pi (i number) (j number) number)
    (pi (i number) (pi (j number) number))
    (lambda (i number) (lambda (j number) (app (app + i) j)))))

(check-compiles
  zero?
  (compiled
    (pi number boolean)
    (pi number boolean)
    zero?))

(check-compiles
  (zero? 1)
  (compiled
    boolean
    boolean
    (app zero? (native 1))))

(check-compiles
  (lambda (t type) t)
  (compiled
    (pi (t type) type)
    (pi (t type) type)
    (lambda (t type) t)))

; === pi

(check-compiles
  (pi number boolean)
  (compiled
    type
    type
    (pi number boolean)))

(check-compiles
  (pi (x type) x)
  (compiled
    type
    type
    (pi (x type) x)))

(check-compiles
  (pi (x type) (y type) x)
  (compiled
    type
    type
    (pi (x type) (pi (y type) x))))

; === if

(check-compiles
  (if (zero? 0) "zero" "not-zero")
  (compiled
    string
    string
    (if (app zero? (native 0)) (native "zero") (native "not-zero"))))

(check-compile-raises (if "not boolean" "zero" "not-zero"))
(check-compile-raises (if #t 10 "not-number"))

; === identity

(check-compiles
  (let
    (identity (lambda (t type) (lambda (x t) t)))
    ((identity number) 10))
  (compiled
    type
    type
    (let
      (identity (lambda (t type) (lambda (x t) t)))
      (app (app identity number) (native 10)))))

; === macro

(check-compiles
  (macro (c t) t)
  (compiled
    macro
    (macro (c t) t)
    (native #f)))

(check-compiles
  (let
    (pi (macro (c t) 3.14))
    (pi some random params))
  (compiled
    number
    number
    (let (pi (native #f)) (native 3.14))))

; === custom types

(check-compiles
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
    (fx-id (fx 10)))
  (compiled #f #f
    (let (fx (native #f))
      (let (fx-10 (tagged (constant fx) 10))
        (let (fx-id (lambda (x (constant fx)) x))
          (app fx-id (tagged (constant fx) 10)))))))

; === dependent identity

(check-compiles
  (let
    (id (lambda (t type) (x t) x))
    (id number 12))
  (compiled number number
    (let (id (lambda (t type) (lambda (x t) x)))
      (app (app id number) (native 12)))))


