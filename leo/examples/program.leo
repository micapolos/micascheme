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

display-line "=== This is produced by program.leo ==="
display-line hello-world
display-line "=== End of program.leo ==="
