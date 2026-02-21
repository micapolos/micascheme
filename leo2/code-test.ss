(import (leo2 base) (leo2 code))

; === atom

(check (atom? #t))
(check (atom? #f))

(check (atom? 0))
(check (atom? 1))
(check (atom? -1))
(check (atom? 3.14))

(check (atom? #\nul))
(check (atom? #\space))
(check (atom? #\a))

(check (atom? ""))
(check (atom? "foo"))
(check (atom? "12345678901234"))
(check (not (atom? "123456789012345")))  ; (= max-atom-string-length 14)

(check (atom? 'foo))

(check (not (atom? '(1 2 3))))

(check-atom->code=? #t "#t")
(check-atom->code=? #f "#f")

(check-atom->code=? 0 "0")
(check-atom->code=? 1 "1")
(check-atom->code=? -1 "-1")
(check-atom->code=? 3.14 "3.14")

(check-atom->code=? #\nul "#\\nul")
(check-atom->code=? #\space "#\\space")
(check-atom->code=? #\a "#\\a")

(check-atom->code=? "" "\"\"")
(check-atom->code=? "foo" "\"foo\"")

(check-atom->code=? 'foo "foo")

; === phrase

(check (not (phrase? '())))
(check (phrase? '(the lucky 7)))
(check (phrase? '(the first #\a)))
(check (phrase? '(the big "world")))
(check (phrase? '(1 2 3 4 5 6 7)))

(check (not (phrase? '(the lucky (the string)))))
(check (not (phrase? '(1 2 3 4 5 6 7 8))))   ; (= max-phrase-atom-count 7)

(check-phrase->code=? '(the lucky 7) "the lucky 7")
(check-phrase->code=? '(the big "world") "the big \"world\"")
(check-phrase->code=? '(the first #\a) "the first #\\a")

; === line

