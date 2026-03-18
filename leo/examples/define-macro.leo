import
  leo scheme
  math

define-macro
  when
    magic:
    quote void
  when
    magic: a
    subtract: 0, a
  when
    magic: a, b
    add: a, b
  when
    magic: a, b, c
    string-append: a, b, c

write << zero arguments >> magic:
write << single argument >> magic 10
write << two arguments >> magic: 10, 20
write << three arguments >> magic: "1", "2", "3"
