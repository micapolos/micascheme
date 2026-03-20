(import (scheme) (check) (condition))

(check
  (equal?
    (condition->datum
      (make-syntax-violation #'foo #f))
    '(syntax-violation foo #f)))
