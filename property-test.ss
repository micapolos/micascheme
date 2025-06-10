(import (scheme) (check) (property))

(let
  (($property (property a 10)))
  (check (free-identifier=? (car $property) #'a))
  (check (equal? (cdr $property) 10)))

(let
  (($properties (properties (a 10) (b 20))))
  (check (equal? (length $properties) 2))
  (check (free-identifier=? (car (car $properties)) #'a))
  (check (equal? (cdr (car $properties)) 10))
  (check (free-identifier=? (car (cadr $properties)) #'b))
  (check (equal? (cdr (cadr $properties)) 20)))
