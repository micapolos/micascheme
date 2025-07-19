(import (micascheme) (asm-3 sized) (asm-3 located) (asm-3 sized-located))

(check
  (equal?
    (sized-list->located-list
      (list
        (sized 2 "a")
        (sized 4 "b")
        (sized 5 "c")))
    (list
      (located 0 "a")
      (located 2 "b")
      (located 6 "c"))))
