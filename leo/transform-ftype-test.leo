import
  scheme
  check
  syntax
  leo transform-ftype

check syntax=?
  transform-ftype syntax u8
  syntax u8

check syntax-datum=?
  transform-ftype
    syntax bits
      sign unsigned 1
      data unsigned 7
  syntax bits
    sign: unsigned, 1
    data: unsigned, 7

check syntax=?
  transform-ftype
    syntax struct
      x int
      y bits sign unsigned 1
  syntax struct
    x int
    y bits sign: unsigned, 1

check syntax=?
  transform-ftype
    syntax union
      x int
      y bits sign unsigned 1
  syntax union
    x int
    y bits sign: unsigned, 1

check syntax=?
  transform-ftype
    syntax array: 16, bits sign unsigned 1
  syntax array
    16
    bits sign: unsigned, 1
