(import (scheme) (option) (check) (monad-syntax))

(check (equal? (option 123) 123))
(check (raises? (lambda () (option #f))))

(check (equal? (pure (option 123)) 123))

(check (equal? (bind (option $value 123) (fx1+ $value)) 124))
(check (equal? (bind (option $value #f) (fx1+ $value)) #f))
(check (equal? (bind (option $value 123) #f) #f))
(check (equal? (bind (option $value #f) #f) #f))

(check (equal? (fmap option fx1+ 123) 124))
(check (equal? (fmap option fx1+ #f) #f))
