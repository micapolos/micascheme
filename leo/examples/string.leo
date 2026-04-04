import leo scheme

define hello "Hello"
define leo "Leo"

define
  exclamated string
  string-append: string, "!"

define
  comma-separated: string-a, string-b
  string-append
    string-a
    ", "
    string-b

display comma-separated
  hello
  exclamated leo
