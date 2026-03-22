import
  leo scheme
  math

define-macro
  when
    magic:
    << zero arguments greet >> "Hello, world!"
  when
    magic: a
    << single argument negates >> subtract: 0, a
  when
    magic: a, b
    << two arguments add >> add: a, b
  when
    magic: a, ...
    << more arguments append >> string-append: a, ...

pretty-print magic:
pretty-print magic: 10
pretty-print magic: 10, 20
pretty-print magic: "a", "b", "c", "d", "e"
