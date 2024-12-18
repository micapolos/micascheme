(import (scheme) (check) (throw) (list))

; === or-throw ===

(check
  (equal?
    (or-throw "foo")
    "foo"))

(check
  (raises
    (or-throw #f)))

(check
  (equal?
    (or-throw (single (list 1)))
    1))

(check
  (raises
    (or-throw (single (list)))))

