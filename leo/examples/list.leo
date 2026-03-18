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

write map
  lambda
    with listed-string
    string-append
      listed-string
      ", ("
      number->string string-length listed-string
      ")"
  list
    "this"
    "is"
    "a"
    "list"
    "of"
    "strings"
