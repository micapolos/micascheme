(import (leo2 base) (leo2 code))

; === datum-length-max?

(check (equal? (datum-length-max? '() 7) 1))
(check (equal? (datum-length-max? "foo" 7) 1))

(check (equal? (datum-length-max? '(a b c) 4) 3))
(check (equal? (datum-length-max? '(a b c d) 4) 4))
(check (equal? (datum-length-max? '(a b c d e) 4) #f))

(check (equal? (datum-length-max? '(a (b c d) e) 6) 5))
(check (equal? (datum-length-max? '(a (b c d) e f) 6) 6))
(check (equal? (datum-length-max? '(a (b c d) e f g) 6) #f))

; === atom

(check (atom? '()))

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

(check-atom->code=? '() "()")

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

(check (phrase? '()))
(check (phrase? '(the lucky 7)))
(check (phrase? '(the first #\a)))
(check (phrase? '(the big "world")))
(check (phrase? '(1 2 3 4 5 6 7)))

(check (not (phrase? '(the lucky (the string)))))
(check (not (phrase? '(1 2 3 4 5 6 7 8))))   ; (= max-line-atom-count 7)

(check-phrase->code=? '() "()")
(check-phrase->code=? 'foo "foo")
(check-phrase->code=? '(the lucky 7) "the lucky 7")
(check-phrase->code=? '(the big (the small string)) "the big (the small string)")

; === script

