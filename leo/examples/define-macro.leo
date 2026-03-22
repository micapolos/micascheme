import leo scheme

define-macro
  when
    magic:
    null
  when
    magic: a
    string-append: "Hello, ", a, "!"
  when
    magic: a, b
    +: a, b
  when
    magic: a, ...
    string-append: a, ...

pretty-print magic:
pretty-print magic: "world"
pretty-print magic: 10, 20
pretty-print magic: "a", "b", "c", "d", "e"
