(import
  (micalang base)
  (micalang term)
  (micalang compiler))

; === type

(check-compiles any-type (compiled any-type any-type any-type))

; === native

(check-compiles
  (native any-boolean foo)
  (compiled any-boolean any-boolean foo))

(check-compiles
  (let
    (zero? (native (any-lambda any-number any-boolean) (curry %%zero? (x any-number))))
    (zero? 1))
  (compiled any-boolean any-boolean
    (let
      (zero? (curry %%zero? (x any-number)))
      (app zero? (native 1)))))

(check-compiles
  (let
    (add (native (any-lambda any-number any-number any-number) (curry %%+ (a any-number) (b any-number))))
    (increment (a any-number) (+ a 1))
    (double (a any-number) (b any-number) (add a b))
    (double (increment 1)))
  (compiled
    (any-lambda (b any-number) any-number)
    (any-lambda (b any-number) any-number)
    (let (add (curry %%+ (a any-number) (b any-number)))
      (let (increment (lambda (a any-number) (app (app + a) (native 1))))
        (let (double (lambda (a any-number) (lambda (b any-number) (app (app add a) b))))
          (app double (app increment (native 1))))))))

(check-compiles
  (native (any-lambda any-number any-number any-number) (native (%lambda (x) (%lambda (y) (%+ x y)))))
  (compiled
    (any-lambda any-number any-number any-number)
    (any-lambda any-number (any-lambda any-number any-number))
    (native (%lambda (x) (%lambda (y) (%+ x y))))))

; === natives

(check-compiles #f (compiled any-boolean any-boolean (native #f)))
(check-compiles #t (compiled any-boolean any-boolean (native #t)))
(check-compiles 123 (compiled any-number any-number (native 123)))
(check-compiles 3.14 (compiled any-number any-number (native 3.14)))
(check-compiles 'foo (compiled any-symbol any-symbol (native 'foo)))
(check-compiles #\a (compiled any-char any-char (native #\a)))
(check-compiles "foo" (compiled any-string any-string (native "foo")))

; === globals

(check-compiles any-boolean (compiled any-type any-type any-boolean))
(check-compiles any-number (compiled any-type any-type any-number))
(check-compiles any-string (compiled any-type any-type any-string))

(check-compiles
  zero?
  (compiled
    (any-lambda any-number any-boolean)
    (any-lambda any-number any-boolean)
    zero?))

(check-compiles
  <
  (compiled
    (any-lambda any-number any-number any-boolean)
    (any-lambda any-number any-number any-boolean)
    <))

; === application

(check-compiles
  (zero? 10)
  (compiled
    any-boolean
    any-boolean
    (app zero? (native 10))))

(check-compiles
  ((+ 10) 20)
  (compiled
    any-number
    any-number
    (app (app + (native 10)) (native 20))))

(check-compiles
  (+ 10 20)
  (compiled
    any-number
    any-number
    (app (app + (native 10)) (native 20))))

(check-compile-raises (zero? #t))
(check-compile-raises (+ 1 #t))
(check-compile-raises (+ #t 2))

; === let

(check-compiles
  (let 10)
  (compiled any-number any-number (native 10)))

(check-compiles
  (let (x 10) (zero? x))
  (compiled any-boolean any-boolean (let (x (native 10)) (app zero? x))))

(check-compiles
  (let (x 10) (y 20) (< x y))
  (compiled
    any-boolean
    any-boolean
    (let (x (native 10))
      (let (y (native 20))
        (app (app < x) y)))))

(check-compiles
  (let
    (zwiększ (x any-number) (+ x 1))
    (zwiększ 10))
  (compiled
    any-number
    any-number
    (let
      (zwiększ (lambda (x any-number) (app (app + x) (native 1))))
      (app zwiększ (native 10)))))

; === lambda

(check-compiles
  (lambda 12)
  (compiled any-number any-number (native 12)))

(check-compiles
  (lambda (i any-number) i)
  (compiled
    (any-lambda (i any-number) any-number)
    (any-lambda (i any-number) any-number)
    (lambda (i any-number) i)))

(check-compiles
  (lambda (i any-number) (j any-number) (+ i j))
  (compiled
    (any-lambda (i any-number) (j any-number) any-number)
    (any-lambda (i any-number) (any-lambda (j any-number) any-number))
    (lambda (i any-number) (lambda (j any-number) (app (app + i) j)))))

(check-compiles
  zero?
  (compiled
    (any-lambda any-number any-boolean)
    (any-lambda any-number any-boolean)
    zero?))

(check-compiles
  (zero? 1)
  (compiled
    any-boolean
    any-boolean
    (app zero? (native 1))))

(check-compiles
  (lambda (t any-type) t)
  (compiled
    (any-lambda (t any-type) any-type)
    (any-lambda (t any-type) any-type)
    (lambda (t any-type) t)))

; === pi

(check-compiles
  (any-lambda any-number any-boolean)
  (compiled
    any-type
    any-type
    (any-lambda any-number any-boolean)))

(check-compiles
  (any-lambda (x any-type) x)
  (compiled
    any-type
    any-type
    (any-lambda (x any-type) x)))

(check-compiles
  (any-lambda (x any-type) (y any-type) x)
  (compiled
    any-type
    any-type
    (any-lambda (x any-type) (any-lambda (y any-type) x))))

; === if

(check-compiles
  (if (zero? 0) "zero" "not-zero")
  (compiled
    any-string
    any-string
    (if (app zero? (native 0)) (native "zero") (native "not-zero"))))

(check-compile-raises (if "not boolean" "zero" "not-zero"))
(check-compile-raises (if #t 10 "not-number"))

; === identity

(check-compiles
  (let
    (identity (lambda (t any-type) (lambda (x t) t)))
    ((identity any-number) 10))
  (compiled
    any-type
    any-type
    (let
      (identity (lambda (t any-type) (lambda (x t) t)))
      (app (app identity any-number) (native 10)))))

; === macro

(check-compiles
  (macro (c t) t)
  (compiled
    macro
    (macro (c t) t)
    (native #f)))

(check-compiles
  (let
    (any-lambda (macro (c t) 3.14))
    (any-lambda some random params))
  (compiled
    any-number
    any-number
    (let (any-lambda (native #f)) (native 3.14))))

; === custom types

(check-compiles
  (let
    (any-fx (native any-type (constant any-fx)))
    (fx (macro (c t)
      (%%syntax-case t ()
        ((_ n)
          (%%if (%%fixnum? (%%datum n))
            `(native any-fx (tagged (constant any-fx) ,(%%datum n)))
            (%%syntax-error #'n "not fixnum"))))))
    (fx-10 (fx 10))
    (fx-id (lambda (x any-fx) x))
    (fx-id (fx 10)))
  (compiled any-fx any-fx
    (let (any-fx (constant any-fx))
      (let (fx (native #f))
        (let (fx-10 (tagged (constant any-fx) 10))
          (let (fx-id (lambda (x any-fx) x))
            (app fx-id (tagged (constant any-fx) 10))))))))

; === dependent identity

(check-compiles
  (let
    (id (lambda (t any-type) (x t) x))
    (id any-number 12))
  (compiled any-number any-number
    (let (id (lambda (t any-type) (lambda (x t) x)))
      (app (app id any-number) (native 12)))))
