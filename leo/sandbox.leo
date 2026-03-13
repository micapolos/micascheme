import
  leo scheme
  only
    less?, add, subtract
    from leo math

define hello "Hello"
define world "world"

define
  exclamated string
  string-append: string, "!"

define
  comma-separated: string-a, string-b
  string-append: string-a, ", ", string-b

displayln comma-separated
  hello
  exclamated world

displayln time let
  fibonacci
  : number 45
  if
    less?: number, 2
    number
    add
      fibonacci subtract: number, 1
      fibonacci subtract: number, 2
