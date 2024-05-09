(import (micascheme) (minic type))

(check (equal? (type->datum (type-type)) 'type))
(check (equal? (type->datum (syntax-type)) 'syntax))
(check (equal? (type->datum (int-type 5)) 'u5))
(check (equal? (type->datum (ref-type (int-type 5))) '(& u5)))

(check
  (equal?
    (type->datum
      (function-type
        (list (int-type 8) (int-type 3))
        (int-type 4)))
    '(-> (u8 u3) u4)))
