(import
  (tt lang)
  (tt number)
  (tt boolean)
  (tt datum)
  (prefix (scheme) %))

(define-record (point (x number) (y number)))

(check
  (point->datum (point 10 20))
  (typed datum (point 10 20)))

(check
  (point=?
    (point 10 10)
    (point 10 10))
  (typed boolean #t))

(check
  (point=?
    (point 10 10)
    (point 10 11))
  (typed boolean #f))

(print (point 10 20))
(print (point->datum (point 10 20)))
