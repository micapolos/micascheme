import leo scheme

write << number >> list
  "this"
  "is"
  "a"
  "list"
  "of"
  "strings"

write << string >> map
  number->string
  list: 1, 2, 3, 4

write << odd? >> map
  odd?
  list: 1, 2, 3, 4

write << odd >> filter
  odd?
  iota 20

define
  string-append-length string
  string-append
    string
    " ("
    number->string string-length string
    ")"

write << strings with length >> map
  string-append-length
  list
    "this"
    "is"
    "a"
    "list"
    "of"
    "strings"
