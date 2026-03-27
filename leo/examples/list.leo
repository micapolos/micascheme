import leo scheme

write `fruit` list
  "apple"
  "banana"
  "orange"

write `fruit and animal` list
  "apple"
  "banana"
  "orange"
  and list
    "cat"
    "dog"
    "tiger"

write `closed` closed-list
  "apple"
  "banana"
  "orange"

write `open` open-list
  "apple"
  "banana"
  "orange"
  list
    "cat"
    "dog"
    "tiger"

write `number to string` map
  number->string
  list: 1, 2, 3, 4

write `odd?` map
  odd?
  list: 1, 2, 3, 4

write `odd` filter
  odd?
  iota 20

write `even` filter
  even?
  iota 20

define
  string-append-length string
  string-append
    string
    " ("
    number->string string-length string
    ")"

write `strings with length` map
  string-append-length
  list
    "this"
    "is"
    "a"
    "list"
    "of"
    "strings"
