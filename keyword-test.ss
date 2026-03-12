(import (scheme) (check) (keyword) (boolean))

(check (equal? (keyword? foo) #t))
(check (equal? (keyword? 'foo) #f))
(check (equal? (keyword? "foo") #f))
(check (equal? (keyword? 123) #f))
(check (equal? (keyword? '(foo bar)) #f))

(check (free-identifier=? (keyword-append here +) (keyword +)))
(check (free-identifier=? (keyword-append here string - append) (keyword string-append)))

(check (equal? (keyword-replace plus + plus) +))
(check (equal? (keyword-replace plus + (plus 1 2)) 3))
(check (equal? (keyword-replace plus + (plus 1 (plus 2 3))) 6))
