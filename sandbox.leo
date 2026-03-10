import leo lang
define hello "Hello"
define world "world"
define
  exclamated string
  string-append: string, "!"
define
  comma-separated: string-a, string-b
  string-append: string-a, ", ", string-b
displayln comma-separated: hello, exclamated world
