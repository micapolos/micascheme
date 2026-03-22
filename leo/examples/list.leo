import leo scheme

pretty-print << string >> list
  "this"
  "is"
  "a"
  "list"
  "of"
  "strings"

pretty-print << number string >> map
  number->string
  list: 1, 2, 3, 4

pretty-print << odd? >> map
  odd?
  list: 1, 2, 3, 4

pretty-print << odd >> filter
  odd?
  iota 20

pretty-print << even >> filter
  even?
  iota 20

define
  string-append-length string
  string-append
    string
    " ("
    number->string string-length string
    ")"

pretty-print << strings with length >> map
  string-append-length
  list
    "this"
    "is"
    "a"
    "list"
    "of"
    "strings"
