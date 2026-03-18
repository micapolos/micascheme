import
  leo scheme
  math
  only
    intercalate
    from micascheme

define hello "Hello"
define world "world"

define
  exclamated string
  string-append: string, "!"

define
  comma-separated: strings ...
  apply
    string-append
    intercalate: strings, ", "

displayln string-append comma-separated
  hello
  exclamated world
