import leo scheme

define hello "Hello"
define world "world"

define lambda
  exclamated string
  string-append: string, "!"

define lambda
  comma-separated: string-a, string-b
  string-append
    string-a
    ", "
    string-b

display-line comma-separated
  hello
  exclamated world
