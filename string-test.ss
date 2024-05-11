(import (scheme) (check) (string))

(check (equal? (lines-string) ""))
(check (equal? (lines-string "foo") "foo\n"))
(check (equal? (lines-string "foo" "bar") "foo\nbar\n"))
(check (equal? (apply lines-string (list "foo" "bar")) "foo\nbar\n"))
