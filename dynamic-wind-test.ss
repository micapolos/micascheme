(import (scheme) (check) (dynamic-wind))

(check
  (equal?
    (with-dynamic-wind "OK")
    "OK"))

(check
  (equal?
    (with-dynamic-wind
      ($alloc-1 (foreign-alloc 1))
      "OK"
      (foreign-free $alloc-1))
    "OK"))

(check
  (equal?
    (with-dynamic-wind
      ($alloc-1 (foreign-alloc 1))
      ($alloc-2 (foreign-alloc 2))
      "OK"
      (foreign-free $alloc-2)
      (foreign-free $alloc-1))
    "OK"))
