(import (micascheme) (minic type))

(check (equal? (type->datum (int-type 5)) '(int 5)))
(check (equal? (type->datum (array-type (int-type 8) 5)) '(array (int 8) 5)))
(check (equal? (type->datum (ref-type (int-type 5))) '(& (int 5))))

(check
  (equal?
    (type->datum
      (function-type
        (list (int-type 8) (int-type 3))
        (int-type 4)))
    '(-> ((int 8) (int 3)) (int 4))))
