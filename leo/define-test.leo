import
  except: define, lambda, from scheme
  leo check
  leo lambda
  leo define

define
  value zero 0

  lambda
    increment x
    +: x, 1

  value one increment zero

  two increment one

  lambda
    multiply-add: a, b, c
    +
      *: a, b
      c

  syntax
    when
      number-magic: a
      +: a, 1
    when
      number-magic: a, b
      +: a, b
    when
      number-magic: a, ...
      *: a, ...
    when
      string-magic: a, ...
      string-append: a, ..., "!"

check equal?: zero, 0
check equal?: one, 1
check equal?: two, 2
check equal?: increment 10, 11

check equal?
  multiply-add: 10, 20, 30
  230

check equal?
  number-magic: 10
  11

check equal?
  number-magic: 10, 20
  30

check equal?
  number-magic: 10, 20, 30
  6000

check equal?
  string-magic: "foo", "bar"
  "foobar!"
