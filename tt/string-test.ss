(import (tt lang) (tt string) (tt number) (tt boolean) (tt list))

(check (string=? "foo" "foo"))
(check (not (string=? "foo" "bar")))

(check (number=? (string-length "foo") 3))

(check (string=? (string-append "foo" "bar") "foobar"))

(check (string=? (string-join (list "foo" "bar" "goo")) "foobargoo"))
(check (string=? (string-join (list)) ""))

(check (string=? (number->string 10) "10"))
