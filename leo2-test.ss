(import (micascheme) (leo2))

(define static-type-a (struct! a))
(define static-type-b (struct! b))
(define static-type-c (struct! c))
(define static-type static-type-a)

(define dynamic-type-a (enum! a static-type-a static-type-b))
(define dynamic-type-b (enum! b static-type-a static-type-b))
(define dynamic-type-c (enum! c static-type-a static-type-b))
(define dynamic-type dynamic-type-a)

; type-dynamic? type-static?

(check (type-dynamic? anything!))
(check (type-dynamic? number!))
(check (type-dynamic? string!))

(check (type-static? (struct! foo)))
(check (type-static? (struct! foo static-type)))
(check (type-static? (struct! foo static-type static-type)))
(check (type-dynamic? (struct! foo static-type dynamic-type)))

(check (type-static? (enum! foo static-type)))
(check (type-dynamic? (enum! foo dynamic-type)))
(check (type-dynamic? (enum! foo static-type static-type)))

(check (type-dynamic? (variable 0)))

(check (type-static? (arrow! (dynamic-type))))
(check (type-static? (arrow! (dynamic-type) static-type)))
(check (type-dynamic? (arrow! (dynamic-type) dynamic-type)))

; types-indexing

(check 
  (equal?
    (types-indexing (list)) 
    (indexing 0 (list))))

(check
  (equal?
    (types-indexing (list dynamic-type static-type dynamic-type dynamic-type static-type))
    (indexing 3 (list 0 #f 1 2 #f))))
