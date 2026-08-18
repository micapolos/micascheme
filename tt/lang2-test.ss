(import
  (tt lang2)
  (prefix (scheme) %))

(define-global + %+)

(print (call number + 10 20))
(print ((lambda ((x number) (y number)) (call number + x y)) 10 20))
