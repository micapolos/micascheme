import
  except: define, lambda, cond, predicate, from scheme
  leo check
  leo lambda
  leo define
  leo with
  leo cond
  leo getter-leo
  leo get
  leo predicate
  leo adjective

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

  union id: number, string

  getter string lambda
    with identifier
    cond
      when
        symbol=?: syntax->datum identifier, 'size
        syntax string-length
      when
        free-identifier=?: identifier, syntax number
        syntax string->number
      else
        syntax-error identifier

  adjectives: my, very, lucky

  my lucky number +: 3, 4

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

check id? 123
check id? "foo"
check not id? 'foo

check equal?
  getter string size
  string-length

check equal?
  getter string number
  string->number

check equal?
  my-lucky-number
  7
