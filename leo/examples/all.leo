import leo scheme

define-macro
  when
    load-leo: xs, ...
    begin
      begin
        write << === Loading >> quote xs
        load string-append
          symbol->string quote xs
          ".leo"
      ...

load-leo
  bytevector
  char
  command-line-arguments
  data
  define-macro
  eval
  fibonacci
  greeting
  hello
  if
  imports
  library
  list
  logging
  pretty-print
  program
  quote
  sandbox
  scheme-write
  string
  switch
  syntax-case
  syntax
  stop-level-program
  write
