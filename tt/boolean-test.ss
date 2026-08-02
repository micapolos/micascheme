(import (tt lang) (tt boolean))

(check (boolean=? #t #t))

(check (boolean=? (and) #t))
(check (boolean=? (and #t) #t))
(check (boolean=? (and #t #t) #t))
(check (boolean=? (and #t #t #f) #f))

(check (boolean=? (or) #f))
(check (boolean=? (or #f) #f))
(check (boolean=? (or #f #f) #f))
(check (boolean=? (or #f #f #t) #t))
