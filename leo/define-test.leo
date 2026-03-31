import leo scheme

define
  value zero 0

  lambda
    increment x
    add: x, 1

  value one increment zero

  two increment one

  lambda
    multiply-add: a, b, c
    add
      multiply: a, b
      c

check equal?: zero, 0
check equal?: one, 1
check equal?: two, 2
check equal?: increment 10, 11

check equal?
  multiply-add: 10, 20, 30
  230
