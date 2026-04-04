import leo scheme

define hello "Hello"
define leo "Leo"
define exclamation "!"

define
  exclamate string
  string-append: string, exclamation

define
  comma-separated: string-a, string-b
  string-append
    string-a
    ", "
    string-b

define hello-leo
  comma-separated
    hello
    exclamate leo

display "=== This is produced by program.leo ==="
display hello-leo
display "=== End of program.leo ==="
