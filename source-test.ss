(import (scheme) (check) (source))

(check
  (equal?
    (source 123)
    '(source 123
      (in "source-test.ss")
      (at (line 5) (column 13)))))
