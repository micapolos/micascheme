(import
  (prefix (leo2 base) %)
  (prefix (leo2 term) %)
  (leo2 stdlib)
  (leo2 comptime))

(check=?
  (variable string-type x)
  (variable-term string-type 'x))

(check=?
  (type 0)
  (%type 0))

(check=?
  (indexed 6 (symbol car))
  (indexed-term 6 (symbol-term 'car)))

(check=? (literal #f) (boolean-term #f))
(check=? (literal 123) (number-term 123))
(check=? (literal #\a) (char-term #\a))
(check=? (literal "foo") (string-term "foo"))

(check=?
  (symbol thing)
  (symbol-term 'thing))

(check=?
  (a-symbol thing)
  (symbol-type-term 'thing))

(check=?
  (symbolic lucky (native number-type 7))
  (symbolic-term 'lucky (number-term 7)))

(check=?
  (native number-type 10)
  (native-term number-type 10))

(check=?
  (native-apply string-type
    %string-append
    (native string-type "foo")
    (variable string-type x))
  (native-application-term
    string-type
    %string-append
    (native-term string-type "foo")
    (variable-term string-type 'x)))

(check=?
  (lambda (x string-type) x)
  (abstraction-term string-type (%lambda (x) x)))

(check=?
  (lambda (x string-type)
    (lambda (y number-type)
      x))
  (abstraction-term string-type
    (%lambda (x)
      (abstraction-term number-type
        (%lambda (y) x)))))

(check=?
  (lambda (x string-type)
    (lambda (y number-type)
      y))
  (abstraction-term string-type
    (%lambda (x)
      (abstraction-term number-type
        (%lambda (y) y)))))

(check=?
  (a-lambda (_ string-type) boolean-type)
  (abstraction-type-term string-type
    (%lambda (x) boolean-type)))

(check=?
  (a-lambda (x (type 0)) x)
  (abstraction-type-term (%type 0)
    (%lambda (x) x)))

(check=?
  (annotated (symbol good) (literal "milk"))
  (annotated-term
    (symbol-term 'good)
    (string-term "milk")))

; (check=?
;   (recursion fn string-type (lambda x x))
;   (%recursion
;     (%lambda (fn)
;       (%abstraction
;         (%lambda (x)
;           (%application fn x))))))

; (check=?
;   (if
;     (variable boolean-type x)
;     (native string-type "foo")
;     (native string-type "bar"))
;   (%branch
;     (variable-term boolean-type 'x)
;     (native-term string-type "foo")
;     (native-term string-type "bar")))
