(import (asm base) (asm dependencies) (asm dependent) (syntax lookup))

(check-environment
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

(check-environment
  (resolve-dependencies
    (lookup-with (recursive (dependent-with (recursive) "recursive")))
    #'recursive)
  (dependencies
    (identified recursive "recursive")))

(check-dependencies
  (dependencies-without (dependencies a b c) #'b)
  (dependencies a c))

(check-dependencies
  (dependencies-without (dependencies a b c) #'d)
  (dependencies a b c))

(check-dependencies
  (dependencies-without-all
    (dependencies a b c d e)
    (dependencies b e f))
  (dependencies a c d))
