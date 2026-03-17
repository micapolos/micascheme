import
  leo scheme
  only: datum->string, from micascheme

define rest list
  "a"
  "b"
  "c"

displayln datum->string
  quote string-append
    "foo"
    "bar"

displayln datum->string
  quasiquote string-append
    unquote "foo"
    unquote "bar"

displayln datum->string
  quasiquote string-append
    unquote "foo"
    unquote "bar"
    unquote-splicing rest

displayln datum->string syntax->datum
  syntax string-append
    "foo"
    "bar"

displayln datum->string syntax->datum
  quasisyntax string-append
    unsyntax "foo"
    unsyntax "bar"

displayln datum->string syntax->datum
  quasisyntax string-append
    unsyntax "foo"
    unsyntax "bar"
    unsyntax-splicing rest
