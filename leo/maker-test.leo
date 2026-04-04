import
  only: equal?, string, make-string, from scheme
  leo check
  leo define
  leo maker

define make-blah-blah #f

check equal?
  maker string
  make-string

check equal?
  maker blah-blah
  make-blah-blah
