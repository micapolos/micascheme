(import
  (leo2 base)
  (leo2 term)
  (leo2 lang reify))

(check-reify (type 0) kind)
(check-reify (type 1) kind-of-kind)
(check-reify (type 2) kind-of-kind-of-kind)

(check-reify (variable-term string-type 'x) x)

(check-reify (string-term "foo") "foo")

(check-reify (evaluated (string-term "foo")) "foo")

(check-reify
  (native-application-term
    string-type
    string-append
    (string-term "foo")
    (string-term "bar"))
  (native-apply string ,string-append "foo" "bar"))

(check-reify
  (abstraction-term string-type (lambda (x) (string-term "foo")))
  (lambda (_ : string) "foo"))

(check-reify
  (abstraction-term string-type (lambda (x) x))
  (lambda (v0 : string) v0))

(check-reify
  (abstraction-term string-type (lambda (x)
    (abstraction-term string-type (lambda (y)
      (native-application-term string-type string-append x y)))))
  (lambda
    (v0 : string)
    (v1 : string)
    (native-apply string ,string-append v0 v1)))

; (check-reify
;   (application-term
;     (application-term
;       (application-term
;         (abstraction-term string-type (lambda (x)
;           (abstraction-term number-type (lambda (y)
;             (abstraction-term char-type (lambda (z)
;               (string-term "OK")))))))
;         (string-term "foo"))
;       (number-term 10))
;     (char-term #\a))
;   ((((lambda (_ : string) (_ : a-number) (_ : a-char) "OK") "foo") 10) #\a)

(check-reify
  (abstraction-type-term number-type
    (lambda (x) string-type))
  (a-lambda number string))

(check-reify
  (abstraction-type-term number-type
    (lambda (x)
      (abstraction-type-term boolean-type
        (lambda (x) string-type))))
  (a-lambda number boolean string))

(check-reify
  (abstraction-type-term (type 0)
    (lambda (x)
      (native-application-term (type 0) list x)))
  (a-lambda (v0 : kind)
    (native-apply kind ,list v0)))

(check-reify
  (abstraction-type-term (type 0)
    (lambda (x)
      (abstraction-type-term number-type
        (lambda (y)
          (native-application-term (type 0) list x y)))))
  (a-lambda
    (v0 : kind)
    (v1 : number)
    (native-apply kind ,list v0 v1)))

; (check-reify
;   (recursion (lambda (fn) (native "foo")))
;   (recursive lambda _ "foo"))

; (check-reify
;   (recursion (lambda (fn) fn))
;   (recursive lambda v0 v0))

; (check-reify
;   (recursion (lambda (fn)
;     (abstraction (lambda (n)
;       (application fn n)))))
;    (recursive lambda v0 v1 (v0 v1)))

(check-reify
  (branch-term
    (variable-term boolean-type 'x)
    (variable-term string-type 'y)
    (variable-term string-type 'z))
  (if x y z))

(check-reify
  (symbol-term 'thing)
  thing)

(check-reify
  (indexed-term 6 (symbol-term 'thing))
  (thing $7))

(check-reify
  (symbolic-term 'lucky (number-term 7))
  (lucky 7))

(check-reify
  (symbolic-term 'very
    (symbolic-term 'lucky
      (number-term 7)))
  (very lucky 7))

(check-reify
  (symbolic-term 'lucky (symbol-term 'number))
  (lucky number))

(check-reify
  (symbolic-term 'very
    (symbolic-term 'lucky
      (symbol-term 'number)))
  (very lucky number))

(check-reify
  (annotated-term (symbol-term 'good)
    (symbol-term 'milk))
  (annotated good milk))

(check-reify
  (annotated-term (symbol-term 'very)
    (annotated-term (symbol-term 'good)
      (symbol-term 'milk)))
  (annotated very good milk))

(check-reify
  (annotated-term (symbol-term 'really)
    (annotated-term (symbol-term 'very)
      (annotated-term (symbol-term 'good)
        (symbol-term 'milk))))
  (annotated really very good milk))

; ; TODO: Why they do not work?
; (check-reify
;   (binding
;     (native "foo")
;     (lambda (x)
;       (application
;         (application (variable 'string-append) x)
;         (native "!"))))
;   (let
;     (v0 "foo")
;     (string-append v0 "!")))

; (check-reify
;   (binding
;     (native "foo")
;     (lambda (x)
;       (binding
;         (native "bar")
;         (lambda (y)
;           (application
;             (application (variable 'string-append) x)
;             y)))))
;   (let
;     (v0 "foo")
;     (v1 "bar")
;     (string-append v0 v1)))
