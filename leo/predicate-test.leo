import
  only: equal?, string, string?, from scheme
  leo define
  leo check
  leo predicate

define blah-blah? #f

check equal?
  predicate string
  string?

check equal?
  predicate blah-blah
  blah-blah?
