(import
  (tt lang2)
  (prefix (scheme) %))

(define-global + string %+)

(print (call number + 10 20))
