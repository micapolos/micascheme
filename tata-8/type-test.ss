(import (scheme) (check) (tata-8 type))

(check
  (equal?
    (type->datum
      (symbolic-type 'foo (list integer-type text-type)))
    '(foo integer text)))


; symbolic-type-index-of

(check
  (equal?
    (symbolic-type-index-of
      (symbolic-type 'foo (list integer-type text-type))
      integer-type)
    0))

(check
  (equal?
    (symbolic-type-index-of
      (symbolic-type 'foo (list integer-type text-type))
      text-type)
    1))

(check
  (raises
    (symbolic-type-index-of
      (symbolic-type 'foo (list integer-type text-type))
      drawing-type)))
