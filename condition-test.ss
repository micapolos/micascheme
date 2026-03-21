(import (scheme) (check) (condition))

(check
  (equal?
    (condition->datum
      (make-syntax-violation #'foo #f))
    '(syntax-violation
      foo
      (in "condition-test.ss")
      (at (line 6) (column 32)))))
