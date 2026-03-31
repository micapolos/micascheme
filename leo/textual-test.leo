import
  leo scheme
  leo textual
  leo current

check equal?
  textual-string textual char a
  "a"

check equal?
  textual-string textual "foo"
  "foo"

check equal?
  textual-string char->textual string-ref: "abc", 0
  "a"

check equal?
  textual-string string->textual string-append: "foo", "bar"
  "foobar"

check equal?
  textual-string textual
    with port
    put-string: port, "foo"
  "foo"

check equal?
  textual-string list->textual list
    textual "Hello"
    textual ", "
    textual "Leo"
    textual char !
  "Hello, Leo!"

check equal?
  textual-string textual-append
    textual "Hello"
    textual ", "
    textual "Leo"
    textual char !
  "Hello, Leo!"
