(import
  (tt lang)
  (tt record)
  (tt number)
  (tt boolean)
  (tt datum)
  (prefix (scheme) %))

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
