import leo scheme

define-syntax
  when
    load-leo: xs, ...
    begin
      begin
        write `=== Loading` quote xs
        load string-append
          symbol->string quote xs
          ".leo"
      ...

load-leo
  bytevector
  char
  command-line-arguments
  data
  define-syntax
  eval
  fibonacci
  greeting
  hello
  if
  imports
  library
  list
  logging
  program
  quote
  sandbox
  scheme-write
  string
  switch
  syntax-case
  stop-level-program
  write
