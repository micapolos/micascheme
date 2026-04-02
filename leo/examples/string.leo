import leo scheme

define hello "Hello"
define leo "Leo"

define lambda
  exclamated string
  string-append: string, "!"

define lambda
  comma-separated: string-a, string-b
  string-append
    string-a
    ", "
    string-b

display comma-separated
  hello
  exclamated leo
