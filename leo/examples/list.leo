import leo scheme

print 'fruit' list
  "apple"
  "banana"
  "orange"

print 'fruit and animal' list
  "apple"
  "banana"
  "orange"
  and list
    "cat"
    "dog"
    "tiger"

print 'closed' list
  "apple"
  "banana"
  "orange"

print 'open' list*
  "apple"
  "banana"
  "orange"
  list
    "cat"
    "dog"
    "tiger"

print 'number to string' map
  number->string
  list: 1, 2, 3, 4

print 'odd?'' map
  odd?
  list: 1, 2, 3, 4

print 'odd' filter
  odd?
  iota 20

print 'even' filter
  even?
  iota 20

define
  string-append-length string
  string-append
    string
    " ("
    number->string string-length string
    ")"

print 'strings with length' map
  string-append-length
  list
    "this"
    "is"
    "a"
    "list"
    "of"
    "strings"
