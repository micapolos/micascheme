(import (scheme) (check) (infix))

(check (equal? (infix "a") "a"))

(check (equal? (infix "abc" string-length) 3))
(check (equal? (infix "abc" string-length ()) 3))
(check (equal? (infix "abc" string-length () number->string) "3"))
(check (equal? (infix "abc" string-length () number->string ()) "3"))

(check (equal? (infix () string-length "abc") 3))

(check (equal? (infix "a" string-append) "a"))
(check (equal? (infix "a" string-append ()) "a"))
(check (equal? (infix "a" string-append "b") "ab"))
(check (equal? (infix "a" string-append (: "b")) "ab"))
(check (equal? (infix "a" string-append (: "b" "c")) "abc"))

(check (equal? (infix ("a" string-append "b") string-append ("c" string-append "d")) "abcd"))
