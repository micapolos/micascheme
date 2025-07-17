(import (micascheme) (asm-3 identified-stack) (asm-3 fragment) (syntax lookup))

(check-identified-stack
  (identifier->identified-stack
    (lookup-with
      (a (fragment-with () "a"))
      (b (fragment-with () "b"))
      (c (fragment-with () "c"))
      (d (fragment-with () "d"))
      (ab (fragment-with (a b) "ab"))
      (bc (fragment-with (b c) "bc"))
      (ad (fragment-with (a d) "ad"))
      (main (fragment-with (ab bc ad) "main")))
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
