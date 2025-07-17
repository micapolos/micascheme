(import (micascheme) (asm-3 dependent))

(check-dependent
  (pure-dependent 10)
  (dependent-with () 10))

(check-dependent
  (dependent-with (a b) 10)
  (dependent-with (a b) 10))

(check-dependent
  (dependent-append
    (dependent-with (a b) 10)
    (dependent-with (b c) 20)
    (dependent-with (a d) 30))
  (dependent-with (a b c d) '(10 20 30)))
