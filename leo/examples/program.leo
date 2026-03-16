import leo scheme

define hello "Hello"
define world "world"
define exclamation "!"

define
  exclamate string
  string-append: string, exclamation

define comma-separated lambda
  with: string-a, string-b
  string-append
    string-a
    ", "
    string-b

define hello-world
  comma-separated
    hello
    exclamate world

displayln "=== This is produced by program.leo ==="

display hello-world
the newline

displayln "=== End of program.leo ==="
