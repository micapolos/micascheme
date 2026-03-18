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

write magic:
write magic: 10
write magic: 10, 20
write magic: "a", "b", "c", "d", "e"
