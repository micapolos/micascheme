(import (micascheme) (term) (type-term))

(check (equal? (type-term boolean!) (cons 0 #f)))
(check (equal? (type-term number!) (cons 1 #f)))
(check (equal? (type-term string!) (cons 2 #f)))

(check 
  (equal? 
    (type-term (tuple-type! foo string! number!))
    (cons 3 (cons `foo (list (type-term string!) (type-term number!))))))

(check 
  (equal? 
    (type-term (choice-type! string! number!))
    (cons 4 (list (type-term string!) (type-term number!)))))

(check 
  (equal? 
    (type-term (function-type! (foo string! number!) boolean!))
    (cons 5 
      (vector 
        `foo 
        (list (type-term string!) (type-term number!))
        (type-term boolean!)))))
