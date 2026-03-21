(import (scheme) (check) (lim))

(check (make-lim? 10 2))
(check (make-lim? 10 0))
(check (not (make-lim? 10 -1)))

(check (lim=? (make-lim? 10 2) (make-lim? 10 2)))
(check (not (lim=? (make-lim? 10 2) (make-lim? 10 3))))
(check (not (lim=? (make-lim? 10 2) (make-lim? 11 2))))

(check (equal? (lim+? (make-lim? 10 2) 1) (make-lim? 11 1)))
(check (equal? (lim+? (make-lim? 10 2) 2) (make-lim? 12 0)))
(check (not (lim+? (make-lim? 10 2) 3)))
