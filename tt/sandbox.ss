(import
  (tt lang)
  (tt number)
  (tt boolean)
  (tt datum)
  (prefix (scheme) %))

(define-class point)

(define (make-point (x number) (y number))
  (unchecked point (%cons x y)))

(define (point-x (p point))
  (unchecked number (%car p)))

(define (point-y (p point))
  (unchecked number (%cdr p)))

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
  (point->datum (make-point 10 20))
  (typed datum (point 10 20)))

(print (point->datum (make-point 10 20)))
