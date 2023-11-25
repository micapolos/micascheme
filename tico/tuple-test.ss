(import
  (micascheme)
  (tico tuple))

(check (equal? (tuple) #f))
(check (equal? (tuple 'a) 'a))
(check (equal? (tuple 'a 'b) (cons 'a 'b)))
(check (equal? (tuple 'a 'b 'c) (vector 'a 'b 'c)))

(check (equal? (tuple-ref 1 (tuple 'a) 0) 'a))
(check (equal? (tuple-ref 2 (tuple 'a 'b) 0) 'a))
(check (equal? (tuple-ref 2 (tuple 'a 'b) 1) 'b))
(check (equal? (tuple-ref 3 (tuple 'a 'b 'c) 0) 'a))
(check (equal? (tuple-ref 3 (tuple 'a 'b 'c) 1) 'b))
(check (equal? (tuple-ref 3 (tuple 'a 'b 'c) 2) 'c))
