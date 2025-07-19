(import (asm-3 base) (asm-3 dependencies) (asm-3 dependent) (syntax lookup))

(check-dependencies
  (resolve-dependencies
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
  (dependencies
    (identified a "a")
    (identified b "b")
    (identified ab "ab")
    (identified c "c")
    (identified bc "bc")
    (identified d "d")
    (identified ad "ad")
    (identified main "main")))
