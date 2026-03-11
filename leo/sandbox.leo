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
  fib
  n 40
  if
    less?: n, 2
    n
    add
      fib subtract: n, 1
      fib subtract: n, 2
