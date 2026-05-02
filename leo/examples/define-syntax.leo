import leo scheme

define-syntax rules
  when
    magic: a
    subtract: 0, a
  when
    magic: a, b
    add: a, b
  when
    magic: a, b, c
    string-append: a, b, c

print 'single argument' magic 10
print 'two arguments' magic: 10, 20
print 'three arguments' magic: "1", "2", "3"
