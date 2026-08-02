(import (tt lang) (tt string) (tt number) (tt boolean))

(check (string=? "foo" "foo"))
(check (not (string=? "foo" "bar")))

(check (= (string-length "foo") 3))
(check (string=? (string-append "foo" "bar") "foobar"))
(check (string=? (number->string 10) "10"))
