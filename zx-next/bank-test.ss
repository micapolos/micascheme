(import (zx-next test) (zx-next bank))

(test
  (case bank-fill
    (bank-fill #x40 #xbb)
    (assert-word (bank-base) #xbbbb)))
