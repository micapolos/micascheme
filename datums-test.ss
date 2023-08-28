(import (micascheme) (datums))

(check (equal? (is? boolean #t) #t))
(check (equal? (is? boolean #f) #t))
(check (equal? (is? boolean 1) #f))

(check (equal? (is? string "foo") #t))
(check (equal? (is? string 1) #f))

(check (equal? (is? number 1) #t))
(check (equal? (is? number "foo") #f))

(check (equal? (is? point (make point 10 20)) #t))
(check (equal? (is? foo (make point 10 20)) #f))

(check (equal? (make foo #f 10 "foo") `(foo #f 10 "foo")))
(check (equal? (get boolean (make foo #f 10 "foo")) #f))
(check (equal? (get number (make foo #f 10 "foo")) 10))
(check (equal? (get string (make foo #f 10 "foo")) "foo"))

(check (equal? (make point (make x 10) (make y 20)) `(point (x 10) (y 20))))
(check (equal? (get x (make point (make x 10) (make y 20))) `(x 10)))
(check (equal? (get y (make point (make x 10) (make y 20))) `(y 20)))
