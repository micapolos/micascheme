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

pretty-print "=== This is produced by program.leo ==="
pretty-print hello-world
pretty-print "=== End of program.leo ==="
