(import (check) (script))

; === script ===

(check (equal? (script "foo") "foo"))
(check (equal? (script "foo" (string-length)) 3))
(check (equal? (script "foo" (string-append "bar")) "foobar"))
(check (equal? (script "foo" (string-append "bar")) "foobar"))
(check (equal? (script "foo" (string-append "bar") (string-append "goo")) "foobargoo"))
(check (equal? (script "foo" (string-append "bar" "goo")) "foobargoo"))
