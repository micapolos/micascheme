(import (micascheme) (asm typed) (asm block) (syntax lookup))

(check-datum=?
  (block-binary-syntax
    (block
      2
      (stack
        (cons #'a 10)
        (cons #'b 20))
      (stack
        #`(db-binary a)
        #`(db-binary b)))
    100)
  '(let
    ((a 110) (b 120))
    (binary-append
      (db-binary a)
      (db-binary b))))
