import
  leo scheme
  leo textual
  leo current

check equal?
  current-ref current-textual-string textual char a
  "a"

check equal?
  current-ref current-textual-string textual "foo"
  "foo"

check equal?
  current-ref current-textual-string textual
    with port
    put-string: port, "foo"
  "foo"

check equal?
  current-ref current-textual-string list->textual list
    textual "Hello"
    textual ", "
    textual "Leo"
    textual char !
  "Hello, Leo!"

check equal?
  current-ref current-textual-string textual-append
    textual "Hello"
    textual ", "
    textual "Leo"
    textual char !
  "Hello, Leo!"
