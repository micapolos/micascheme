(import (asm-3 base) (asm-3 sized) (asm-3 relocable) (asm-3 sized-relocable))

(check-sized-relocable 100
  (sized 3 (relocable-with ($org) (+ $org 10)))
  (sized 3 110))

(check-sized-relocable 100
  (map-sized list->relocable
    (sized-relocable-append))
  (sized 0 (list)))

(check-sized-relocable 100
  (map-sized list->relocable
    (sized-relocable-append
      (sized 3 (org-relocable))
      (sized 5 (org-relocable))
      (sized 0 (org-relocable))))
  (sized 8 (list 100 103 108)))

(check
  (equal?
    (offset-sized-list cons
      (list
        (sized 3 "foo")
        (sized 6 "foobar")
        (sized 0 "")
        (sized 1 "a")))
    (sized 10
      (list
        (cons 0 "foo")
        (cons 3 "foobar")
        (cons 9 "")
        (cons 9 "a")))))



