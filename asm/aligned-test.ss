(import (micascheme) (asm aligned))

(check
  (aligned-more?
    (aligned 2 "2")
    (aligned 1 "1")))

(check
  (not
    (aligned-more?
      (aligned 1 "1")
      (aligned 1 "1"))))

(check
  (equal?
    (aligned-sort
      (list
        (aligned 1 "1")
        (aligned 8 "8")
        (aligned 2 "2")
        (aligned 4 "4")))
    (list
      (aligned 8 "8")
      (aligned 4 "4")
      (aligned 2 "2")
      (aligned 1 "1"))))

(check
  (equal?
    (aligned-sorted-refs
      (list
        (aligned 1 "1")
        (aligned 8 "8")
        (aligned 2 "2")
        (aligned 4 "4")))
    (list "8" "4" "2" "1")))

(check
  (equal?
    (aligned-append-map string-append
      (aligned 1 "1")
      (aligned 8 "8")
      (aligned 2 "2")
      (aligned 4 "4"))
    (aligned 8 "1824")))

(check
  (equal?
    (aligned-map (aligned 16 "foo") string-length)
    (aligned 16 3)))
