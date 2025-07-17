(import (micascheme) (asm-3 identified-stack) (asm-3 dependent) (syntax lookup))

(check-identified-stack
  (identifier->identified-stack
    (lookup-with
      (a (dependent-with () "a"))
      (b (dependent-with () "b"))
      (c (dependent-with () "c"))
      (d (dependent-with () "d"))
      (ab (dependent-with (a b) "ab"))
      (bc (dependent-with (b c) "bc"))
      (ad (dependent-with (a d) "ad"))
      (main (dependent-with (ab bc ad) "main")))
    #'main)
  (stack
    (identified a "a")
    (identified b "b")
    (identified ab "ab")
    (identified c "c")
    (identified bc "bc")
    (identified d "d")
    (identified ad "ad")
    (identified main "main")))
