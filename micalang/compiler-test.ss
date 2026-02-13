(import
  (micalang base)
  (micalang term)
  (micalang compiler))

; === type

(check-compiles a-type (compiled a-type a-type))

; === native

(check-compiles
  (native a-boolean foo)
  (compiled a-boolean foo))

(check-compiles
  (let
    (zero? (native (a-lambda a-number a-boolean) (curry %%zero? x)))
    (zero? 1))
  (compiled a-boolean
    (let
      (zero? (curry %%zero? x))
      (app zero? (native 1)))))

(check-compiles
  (native-lambda + a-number a-number a-number)
  (compiled
    (a-lambda a-number a-number a-number)
    (curry %%+ v-0 v-1)))

(check-compiles
  (let
    (zero? (native-lambda zero? a-number a-boolean))
    (zero? 1))
  (compiled a-boolean
    (let
      (zero? (curry %%zero? v-0))
      (app zero? (native 1)))))

(check-compiles
  (let
    (add (native-lambda + a-number a-number a-number))
    (increment (a a-number) (+ a 1))
    (double (a a-number) (b a-number) (add a b))
    (double (increment 1)))
  (compiled
    (a-lambda (b a-number) a-number)
    (let (add (curry %%+ v-0 v-1))
      (let (increment (lambda a (app (app + a) (native 1))))
        (let (double (lambda a (lambda b (app (app add a) b))))
          (app double (app increment (native 1))))))))

(check-compiles
  (native (a-lambda a-number a-number a-number) (native (%lambda (x) (%lambda (y) (%+ x y)))))
  (compiled
    (a-lambda a-number a-number a-number)
    (native (%lambda (x) (%lambda (y) (%+ x y))))))

; === natives

(check-compiles #f (compiled a-boolean (native #f)))
(check-compiles #t (compiled a-boolean (native #t)))
(check-compiles 123 (compiled a-number (native 123)))
(check-compiles 3.14 (compiled a-number (native 3.14)))
(check-compiles 'foo (compiled a-symbol (native 'foo)))
(check-compiles #\a (compiled a-char (native #\a)))
(check-compiles "foo" (compiled a-string (native "foo")))

; === globals

(check-compiles a-boolean (compiled a-type a-boolean))
(check-compiles a-number (compiled a-type a-number))
(check-compiles a-string (compiled a-type a-string))

(check-compiles
  zero?
  (compiled
    (a-lambda a-number a-boolean)
    zero?))

(check-compiles
  <
  (compiled
    (a-lambda a-number a-number a-boolean)
    <))

; === application

(check-compiles
  (zero? 10)
  (compiled
    a-boolean
    (app zero? (native 10))))

(check-compiles
  ((+ 10) 20)
  (compiled
    a-number
    (app (app + (native 10)) (native 20))))

(check-compiles
  (+ 10 20)
  (compiled
    a-number
    (app (app + (native 10)) (native 20))))

(check-compile-raises (zero? #t))
(check-compile-raises (+ 1 #t))
(check-compile-raises (+ #t 2))

; === let

(check-compiles
  (let 10)
  (compiled a-number (native 10)))

(check-compiles
  (let (x 10) (zero? x))
  (compiled a-boolean (let (x (native 10)) (app zero? x))))

(check-compiles
  (let (x 10) (y 20) (< x y))
  (compiled a-boolean
    (let (x (native 10))
      (let (y (native 20))
        (app (app < x) y)))))

(check-compiles
  (let
    (zwiększ (x a-number) (+ x 1))
    (zwiększ 10))
  (compiled a-number
    (let
      (zwiększ (lambda x (app (app + x) (native 1))))
      (app zwiększ (native 10)))))

; === lambda

(check-compiles
  (lambda 12)
  (compiled a-number (native 12)))

(check-compiles
  (lambda (i a-number) i)
  (compiled
    (a-lambda (i a-number) a-number)
    (lambda i i)))

(check-compiles
  (lambda (i a-number) (j a-number) (+ i j))
  (compiled
    (a-lambda (i a-number) (j a-number) a-number)
    (lambda i (lambda j (app (app + i) j)))))

(check-compiles
  zero?
  (compiled
    (a-lambda a-number a-boolean)
    zero?))

(check-compiles
  (zero? 1)
  (compiled a-boolean
    (app zero? (native 1))))

(check-compiles
  (lambda (t a-type) t)
  (compiled
    (a-lambda (t a-type) a-type)
    (lambda t t)))

; === pi

(check-compiles
  (a-lambda a-number a-boolean)
  (compiled a-type
    (a-lambda a-number a-boolean)))

(check-compiles
  (a-lambda (x a-type) x)
  (compiled a-type
    (a-lambda (x a-type) x)))

(check-compiles
  (a-lambda (x a-type) (y a-type) x)
  (compiled a-type
    (a-lambda (x a-type) (a-lambda (y a-type) x))))

; === if

(check-compiles
  (if (zero? 0) "zero" "not-zero")
  (compiled a-string
    (if (app zero? (native 0)) (native "zero") (native "not-zero"))))

(check-compile-raises (if "not boolean" "zero" "not-zero"))
(check-compile-raises (if #t 10 "not-number"))

; === identity

(check-compiles
  (let
    (identity (lambda (t a-type) (lambda (x t) t)))
    ((identity a-number) 10))
  (compiled a-type
    (let
      (identity (lambda t (lambda x t)))
      (app (app identity a-number) (native 10)))))

; === macro

(check-compiles
  (macro (c t) t)
  (compiled macro
    (native #f)))

(check-compiles
  (let
    (a-lambda (macro (c t) 3.14))
    (a-lambda some random params))
  (compiled a-number
    (let (a-lambda (native #f)) (native 3.14))))

; === custom types

(check-compiles
  (let
    (a-fx (native a-type (constant a-fx)))
    (fx (macro (c t)
      (%%syntax-case t ()
        ((_ n)
          (%%if (%%fixnum? (%%datum n))
            `(native a-fx (tagged (constant a-fx) ,(%%datum n)))
            (%%syntax-error #'n "not fixnum"))))))
    (fx-10 (fx 10))
    (fx-id (lambda (x a-fx) x))
    (fx-id (fx 10)))
  (compiled a-fx
    (let (a-fx (constant a-fx))
      (let (fx (native #f))
        (let (fx-10 (tagged (constant a-fx) 10))
          (let (fx-id (lambda x x))
            (app fx-id (tagged (constant a-fx) 10))))))))

; === dependent identity

(check-compiles
  (let
    (id (lambda (t a-type) (x t) x))
    (id a-number 12))
  (compiled a-number
    (let (id (lambda t (lambda x x)))
      (app (app id a-number) (native 12)))))
