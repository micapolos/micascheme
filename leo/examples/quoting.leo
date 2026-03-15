import leo scheme

define rest list
  "a"
  "b"
  "c"

pretty-print datum->string
  quote string-append
    "foo"
    "bar"

pretty-print datum->string
  quasiquote string-append
    unquote "foo"
    unquote "bar"

pretty-print datum->string
  quasiquote string-append
    unquote "foo"
    unquote "bar"
    unquote-splicing rest

pretty-print datum->string syntax->datum
  syntax string-append
    "foo"
    "bar"

pretty-print datum->string syntax->datum
  quasisyntax string-append
    unsyntax "foo"
    unsyntax "bar"

pretty-print datum->string syntax->datum
  quasisyntax string-append
    unsyntax "foo"
    unsyntax "bar"
    unsyntax-splicing rest
