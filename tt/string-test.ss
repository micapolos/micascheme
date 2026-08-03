(import (tt lang) (tt string) (tt number) (tt boolean) (tt list))

(check (string=? "foo" "foo"))
(check (not (string=? "foo" "bar")))

(check (number=? (string-length "foo") 3))

(check (string=? (string+ "foo" "bar") "foobar"))

(check (string=? (string-append) ""))
(check (string=? (string-append "1" "2" "3") "123"))

(check (string=? (join-string (list)) ""))
(check (string=? (join-string (list "foo" "bar" "goo")) "foobargoo"))

(check (string=? (list->string (list #\a #\b #\c)) "abc"))

(check (string=? (number->string 10) "10"))
