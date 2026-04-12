import
  leo identifier-syntax
  leo define
  leo check
  leo define-syntax
  leo set
  leo with
  only: equal?, +, -, from scheme

define x 9

define-syntax immutable-x identifier-syntax +: x, 1

define-syntax mutable-x identifier-syntax
  +: x, 1
  v set! x -: v, 1

check equal?
  immutable-x
  10

check equal?
  mutable-x
  10

set! mutable-x 20

check equal?
  x
  19

check equal?
  mutable-x
  20
