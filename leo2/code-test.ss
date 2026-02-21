(import (leo2 base) (leo2 code))

(check-atom->code=? #t "#t")
(check-atom->code=? #f "#f")

(check-atom->code=? 0 "0")
(check-atom->code=? 1 "1")
(check-atom->code=? -1 "-1")
(check-atom->code=? 3.14 "3.14")

(check-atom->code=? #\space "#\\space")
(check-atom->code=? "" "\"\"")
(check-atom->code=? "foo" "\"foo\"")

(check-atom->code=? 'foo "foo")

(check-phrase->code=? '(the lucky 7) "the lucky 7")
