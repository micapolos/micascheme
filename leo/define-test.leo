import
  except: define, from scheme
  leo check
  leo define

define something

define zero 0

define
  increment x
  +: x, 1

define one increment zero
define two increment one

define
  multiply-add: a, b, c
  +
    *: a, b
    c

check equal?: zero, 0
check equal?: one, 1
check equal?: two, 2
check equal?: increment 10, 11

check equal?
  multiply-add: 10, 20, 30
  230
