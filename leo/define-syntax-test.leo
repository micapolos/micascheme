import
  except: define-syntax, case, from scheme
  leo check
  leo case
  leo define-syntax

define-syntax case
  when
    number-magic: a
    syntax +: a, 1
  when
    number-magic: a, b
    syntax +: a, b
  when
    number-magic: a, ...
    syntax *: a, ...
  when
    string-magic: a, ...
    syntax string-append: a, ..., "!"

define-syntax rules
  when
    rules-number-magic: a
    +: a, 1
  when
    rules-number-magic: a, b
    +: a, b
  when
    rules-number-magic: a, ...
    *: a, ...
  when
    rules-string-magic: a, ...
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

check equal?
  rules-number-magic: 10
  11

check equal?
  rules-number-magic: 10, 20
  30

check equal?
  rules-number-magic: 10, 20, 30
  6000

check equal?
  rules-string-magic: "foo", "bar"
  "foobar!"
