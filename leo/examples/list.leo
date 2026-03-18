import leo scheme

write list
  "this"
  "is"
  "a"
  "list"
  "of"
  "strings"

write map
  number->string
  list: 1, 2, 3, 4

write map
  odd?
  list: 1, 2, 3, 4

write filter
  odd?
  iota 20

define
  string-append-length a-string
  string-append
    a-string
    " ("
    number->string string-length a-string
    ")"

write map
  string-append-length
  list
    "this"
    "is"
    "a"
    "list"
    "of"
    "strings"
