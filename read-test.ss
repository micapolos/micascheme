(import (check) (read))

(check
  (equal?
    (syntax->datum (load-syntax-list "read-test-input.ss"))
    '((foo) (bar))))
