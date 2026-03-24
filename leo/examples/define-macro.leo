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
    add: a, b
  when
    magic: a, ...
    string-append: a, ...

write magic:
write magic: "world"
write magic: 10, 20
write magic: "a", "b", "c", "d", "e"
