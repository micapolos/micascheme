import
  leo scheme
  math

define-syntax
  magic: x
  syntax-case
    x
    keywords: foo, bar
    when
      magic: a
      syntax subtract: 0, a
    when
      magic: a, b
      syntax add: a, b
    when
      magic: a, b, c
      syntax string-append: a, b, c

write << single argument negates >> magic 10
write << two arguments adds >> magic: 10, 20
write << three arguments append strings >> magic: "1", "2", "3"
