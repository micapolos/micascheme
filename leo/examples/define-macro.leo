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
    magic: a, b, c
    << three arguments append >> string-append: a, b, c

write magic:
write magic: 10
write magic: 10, 20
write magic: "a", "b", "c"
