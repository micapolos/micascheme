import
  only: zero?, quote, from scheme
  void
  leo check
  leo error

check void? assert zero? 0
check raises assert zero? 123
check raises assertion-violation 'zero? 123
check raises error 'not good
