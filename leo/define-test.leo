import
  except: define, lambda, from scheme
  leo check
  leo lambda
  leo define
  leo with

define
  lambda make-zero 0

  value zero with make-zero

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

  record point: x, y
  union id: number, string

check equal?: zero, 0
check equal?: one, 1
check equal?: two, 2
check equal?: increment 10, 11

check equal?: with make-zero, 0

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

check record? point: 10, 20

check equal?
  point-x point: 10, 20
  10

check equal?
  point-y point: 10, 20
  20

check id? 123
check id? "foo"
check not id? 'foo
