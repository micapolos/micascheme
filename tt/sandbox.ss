(import
  (tt lang)
  (tt number)
  (tt boolean)
  (tt datum)
  (prefix (scheme) %))

(define-record (point (x number) (y number)))

(define (point=? (p1 point) (p2 point))
  (and
    (number=? (point-x p1) (point-x p2))
    (number=? (point-y p1) (point-y p2))))

(check
  (point->datum (point 10 20))
  (typed datum (point 10 20)))

(print (point 10 20))
(print (point->datum (point 10 20)))
