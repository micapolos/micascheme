(import (check) (labs syntax))

; syntax-flatten
(check
  (equal?
    (map syntax->datum (syntax-flatten #'a))
    '(a)))

(check
  (equal?
    (map syntax->datum (syntax-flatten #'(begin)))
    '()))

(check
  (equal?
    (map syntax->datum (syntax-flatten #'(begin a b c)))
    '(a b c)))

(check
  (equal?
    (map syntax->datum (syntax-flatten #'(begin (begin a b) c)))
    '((begin a b) c)))

; flat-map-syntax
(check
  (equal?
    (map syntax->datum
      (flat-map-syntax
        (lambda ($syntax) #`(mapped #,$syntax))
        #'(begin
          (begin)
          1
          (begin 2 3)
          (begin (begin 4 5) (begin 6 7))
          (8 (begin 9)))))
    `(
      (mapped 1)
      (mapped 2)
      (mapped 3)
      (mapped 4)
      (mapped 5)
      (mapped 6)
      (mapped 7)
      (mapped (8 (begin 9))))))
