(import (micascheme) (asm-3 fragment))

(check-fragment
  (pure-fragment 10)
  (fragment-with () 10))

(check-fragment
  (fragment-with (a b) 10)
  (fragment-with (a b) 10))

(check-fragment
  (fragment-append
    (fragment-with (a b) 10)
    (fragment-with (b c) 20)
    (fragment-with (a d) 30))
  (fragment-with (a b c d) '(10 20 30)))
