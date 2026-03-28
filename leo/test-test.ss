(import (scheme) (check) (leo test))

(check
  (equal?
    (test-path foo)
    "foo-test.leo"))

(check
  (equal?
    (test-path (foo bar))
    "foo/bar-test.leo"))

(check
  (equal?
    (test-path (foo (bar zar)))
    "foo/bar/zar-test.leo"))
