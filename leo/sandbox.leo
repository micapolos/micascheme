import leo lang
import leo math
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
displayln time lets-recursive
  fibonacci
  number 45
  if
    fx-less?: number, 2
    number
    fx-add/wraparound
      fibonacci fx-subtract/wraparound: number, 1
      fibonacci fx-subtract/wraparound: number, 2
