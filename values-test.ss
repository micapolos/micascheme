(import (scheme) (check) (values) (list))

(check
  (equal?
    (with-values (values "1" "2" "3") ($1 $2 $3) (string-append $1 $2 $3))
    "123"))

(check
  (equal?
    (values->list (values-append (3 (values "1" "2" "3"))))
    (list "1" "2" "3")))

(check
  (equal?
    (values->list (values-append (1 "1") (1 "2") (1 "3")))
    (list "1" "2" "3")))

(check
  (equal?
    (values->list (values-append (0 (values)) (1 "1") (2 (values "2" "3"))))
    (list "1" "2" "3")))
