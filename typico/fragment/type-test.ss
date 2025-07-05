(import (typico base) (typico type) (typico fragment) (typico fragment type))

(check
  (equal?
    (type->fragment (primitive-type 'id 'name))
    (fragment
      (import (typico type) (scheme))
      (primitive-type 'id 'name))))

(check
  (equal?
    (type->fragment
      (function-type
        (list
          (primitive-type 'id-a 'name-a)
          (primitive-type 'id-b 'name-b))
        (primitive-type 'id-c 'name-c)))
    (fragment
      (import (typico type) (scheme))
      (function-type
        (list
          (primitive-type 'id-a 'name-a)
          (primitive-type 'id-b 'name-b))
        (primitive-type 'id-c 'name-c)))))

(check
  (equal?
    (type->fragment
      (function-type
        (list*
          (primitive-type 'id-a 'name-a)
          (primitive-type 'id-b 'name-b))
        (primitive-type 'id-c 'name-c)))
    (fragment
      (import (typico type) (scheme))
      (function-type
        (list*
          (primitive-type 'id-a 'name-a)
          (primitive-type 'id-b 'name-b))
        (primitive-type 'id-c 'name-c)))))
