(import (scheme) (check) (dynamic-wind))

(check
  (equal?
    (with-dynamic-wind
      ($alloc (foreign-alloc 1))
      "foo"
      "bar"
      (foreign-free $alloc))
    "bar"))
