(import
  (tt lang)
  (tt record)
  (tt number)
  (tt boolean)
  (tt datum)
  (prefix (scheme) %))

; === unit ===

(define-record unit)
(define (unit=? (x unit) (y unit)) #t)
(define (unit->datum (x unit)) 'unit)

; === token ===

(define-record (token))
(define (token=? (x token) (y token)) #t)
(define (token->datum (x token)) 'token)

; === id ===

(define-record (id (number number)))

(define (id=? (x id) (y id))
  (=
    (id-number x)
    (id-number y)))

(define (id->datum (x id))
  (datum-append 'id
    (number->datum (id-number x))))

; === box ===

(define-record (box (forall ref) (ref ref)))

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
    (x number)
    (y number)))

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
    (r number)
    (g number)
    (b number)))

; === pair ===

(define-record
  (pair
    (forall x y)
    (left x)
    (right y)))

