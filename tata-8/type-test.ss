(import (scheme) (check) (tata-8 type))

(check
  (equal?
    (type->datum
      (symbolic-type 'foo (list integer-type text-type)))
    '(foo integer text)))
