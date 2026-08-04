(import
  (tt lang)
  (tt record)
  (tt number)
  (tt boolean)
  (tt datum)
  (tt string)
  (prefix (scheme) %))

; === define-record-constructor ===

(define-class (vec0 _))
(define-class (vec1 _))
(define-class (vec2 _))
(define-class (vec3 _))

(define-record-constructor vec0 (forall (t) (pi () (vec0 t))))
(define-record-constructor vec1 (forall (t) (pi (t) (vec1 t))))
(define-record-constructor vec2 (forall (t) (pi (t t) (vec2 t))))
(define-record-constructor vec3 (forall (t) (pi (t t t) (vec3 t))))

; === define-record-accessor ===

(define-record-accessor vec1-x 0 1 (forall (t) (pi ((vec1 t)) t)))

(define-record-accessor vec2-x 0 2 (forall (t) (pi ((vec2 t)) t)))
(define-record-accessor vec2-y 1 2 (forall (t) (pi ((vec2 t)) t)))

(define-record-accessor vec3-x 0 3 (forall (t) (pi ((vec3 t)) t)))
(define-record-accessor vec3-y 1 3 (forall (t) (pi ((vec3 t)) t)))
(define-record-accessor vec3-z 2 3 (forall (t) (pi ((vec3 t)) t)))

(check (= (vec1-x (vec1 10)) 10))

(check (= (vec2-x (vec2 10 20)) 10))
(check (= (vec2-y (vec2 10 20)) 20))

(check (= (vec3-x (vec3 10 20 30)) 10))
(check (= (vec3-y (vec3 10 20 30)) 20))
(check (= (vec3-z (vec3 10 20 30)) 30))

; === define-union-constructor ===

(define-class (one-of-one _))
(define-class (one-of-two _ _))
(define-class (one-of-three _ _ _))

(define-union-constructor first-of-one 0 1 (forall (a) (pi (a) (one-of-one a))))

(define-union-constructor first-of-two 0 2 (forall (a b) (pi (a) (one-of-two a b))))
(define-union-constructor second-of-two 1 2 (forall (a b) (pi (b) (one-of-two a b))))

(define-union-constructor first-of-three 0 3 (forall (a b c) (pi (a) (one-of-three a b c))))
(define-union-constructor second-of-three 1 3 (forall (a b c) (pi (b) (one-of-three a b c))))
(define-union-constructor third-of-three 2 3 (forall (a b c) (pi (c) (one-of-three a b c))))

; === define-union-matcher ===

(define-union-matcher match-one-of-three
  (forall (a b c)
    (pi ((one-of-three a b c) (pi (a) string) (pi (b) string) (pi (c) string)) string)))

(check
  (string=?
    (match-one-of-three (first-of-three "first")
      (lambda ((s string)) (string-append s "!"))
      (lambda ((n number)) (number->string n))
      (lambda ((ch char)) (string ch)))
    "first!"))

(check
  (string=?
    (match-one-of-three (second-of-three 2)
      (lambda ((s string)) (string-append s "!"))
      (lambda ((n number)) (number->string n))
      (lambda ((ch char)) (string ch)))
    "2"))

(check
  (string=?
    (match-one-of-three (third-of-three #\3)
      (lambda ((s string)) (string-append s "!"))
      (lambda ((n number)) (number->string n))
      (lambda ((ch char)) (string ch)))
    "3"))

; === token ===

(define-record (token))
(define (token=? (x token) (y token)) #t)
(define (token->datum (x token)) 'token)

; === id ===

(define-record (id (id-number number)))

(define (id=? (x id) (y id))
  (=
    (id-number x)
    (id-number y)))

(define (id->datum (x id))
  (datum-append 'id
    (number->datum (id-number x))))

; === box ===

(define-record (box (forall x) (unbox x)))

; TODO: These does not work. Why?
; (define
;   (box=? (forall ref)
;     (ref=? (pi ((box ref) (box ref)) boolean))
;     (b1 (box ref))
;     (b2 (box ref)))
;   (ref=?
;     (box-ref b1)
;     (box-ref b2)))

; (define
;   (box->datum (forall ref)
;     (ref->datum (pi ((box ref)) datum))
;     (b (box ref)))
;   (datum-append 'box
;     (ref->datum (box-ref b))))

; === point ===

(define-record
  (point
    (point-x number)
    (point-y number)))

(define (point=? (p1 point) (p2 point))
  (and
    (= (point-x p1) (point-x p2))
    (= (point-y p1) (point-y p2))))

(define (point->datum (p point))
  (datum-append 'point
    (number->datum (point-x p))
    (number->datum (point-y p))))

(check
  (datum=?
    (point->datum (point 10 20))
    '(point 10 20)))

(check
  (point=?
    (point 10 10)
    (point 10 10)))

(check
  (not
    (point=?
      (point 10 20)
      (point 10 10))))

; === rgb ===

(define-record
  (rgb
    (rgb-r number)
    (rgb-g number)
    (rgb-b number)))

; === pair ===

(define-record (pair (forall x y) (car x) (cdr y)))

(check (= (car (pair 10 "foo")) 10))
(check (string=? (cdr (pair 10 "foo")) "foo"))
