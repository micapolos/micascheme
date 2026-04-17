import
  only: zero?, quote, quasiquote, from scheme
  void
  leo check
  leo error

check void? assert zero? 0
check raises assert zero? 123
check raises assertion-violation 'zero? 123
check raises error 'not good

check raises assertion-violation
  'it aint good
  'what have just happened?
  10
  20
  30

check raises assertion-violation
  'it aint good
  'what have just happened?
  "foo"
  "bar"
