(import (tt lang) (tt char) (tt number))

(check (char=? #\a #\a))
(check (not (char=? #\a #\b)))

(check (= (char->number #\space) 32))
