import
  except: define, define-record, lambda, cond, predicate, from scheme
  leo check
  leo lambda
  leo define
  leo with
  leo cond
  leo getter-leo
  leo get
  leo predicate
  leo record

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

  record type simple-record
    fields: a, b

  record type
    record-1
    fields
      field-1
      immutable field-2
      immutable: field-3, record-1-field-3
      mutable field-4
      mutable: field-5, record-1-field-5, record-1-field-5-set!

  record type
    record-2: make-record-2, record-2?
    fields
      field-1
      immutable field-2
      immutable: field-3, record-2-field-3
      mutable field-4
      mutable: field-5, record-2-field-5, record-2-field-5-set!

  record point: x, y
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

check equal?
  getter string size
  string-length

check equal?
  getter string number
  string->number
