import leo scheme

define hello "Hello"
define world "world"
define exclamation "!"

define lambda
  exclamate string
  string-append: string, exclamation

define lambda
  comma-separated: string-a, string-b
  string-append
    string-a
    ", "
    string-b

define hello-world
  comma-separated
    hello
    exclamate world

display "=== This is produced by program.leo ==="
display hello-world
display "=== End of program.leo ==="
