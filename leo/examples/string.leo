import leo scheme

define hello "Hello"
define world "world"

define
  exclamated string
  string-append: string, "!"

define comma-separated lambda
  with: string-a, string-b
  string-append
    string-a
    ", "
    string-b

pretty-print comma-separated
  hello
  exclamated world
