import
  except: define-syntax, from scheme
  leo check
  leo define-syntax

define-syntax
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
