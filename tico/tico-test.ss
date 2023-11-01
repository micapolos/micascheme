(import (micascheme) (tico tico))

(check
  (equal?
    (tico-load "tico/tico.leo")
    (cons (cons 10 20) 30))) ;TODO: decompile
