import
  leo scheme
  only
    intercalate
    from micascheme

define hello "Hello"
define world "world"

define lambda
  exclamated string
  string-append: string, "!"

define lambda
  comma-separated: and strings
  apply
    string-append
    intercalate: strings, ", "

display-line string-append comma-separated
  hello
  exclamated world
