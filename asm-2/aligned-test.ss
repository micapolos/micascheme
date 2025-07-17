(import (micascheme) (asm-2 aligned))

(check
  (aligned-more?
    (aligned 2 "foo")
    (aligned 1 "bar")))

(check
  (equal?
    (aligned-sort
      (list
        (aligned 1 "a")
        (aligned 8 "b")
        (aligned 2 "c")
        (aligned 4 "d")))
    (list
      (aligned 8 "b")
      (aligned 4 "d")
      (aligned 2 "c")
      (aligned 1 "a"))))
