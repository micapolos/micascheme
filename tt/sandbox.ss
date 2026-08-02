(import
  (tt lang)
  (tt number)
  (tt boolean)
  (tt datum)
  (prefix (scheme) %))

(define-record (point (x number) (y number)))

(define (point=? (p1 point) (p2 point))
  (and
    (= (point-x p1) (point-x p2))
    (= (point-y p1) (point-y p2))))

(define (point->datum (p point))
  (cons
    'point
    (cons
      (number->datum (point-x p))
      (cons
        (number->datum (point-y p))
        '()))))

(check
  (point->datum (point 10 20))
  (typed datum (point 10 20)))

(print (point 10 20))
(print (point->datum (point 10 20)))
