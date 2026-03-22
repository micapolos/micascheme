import leo scheme

define-syntax
  magic: input-syntax
  syntax-case
    input-syntax
    keywords:
    when
      magic: a
      syntax subtract: 0, a
    when
      magic: a, b
      syntax add: a, b
    when
      magic: a, b, c
      syntax string-append: a, b, c

pretty-print << single argument >> magic 10
pretty-print << two arguments >> magic: 10, 20
pretty-print << three arguments >> magic: "1", "2", "3"
