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
  << string-append
    >> "foo"
    >> "bar"

displayln datum->string
  << string-append
    >> "foo"
    >> "bar"
    >>* rest

displayln datum->string syntax->datum
  syntax string-append
    "foo"
    "bar"

displayln datum->string syntax->datum
  <<< string-append
    >>> "foo"
    >>> "bar"

displayln datum->string syntax->datum
  <<< string-append
    >>> "foo"
    >>> "bar"
    >>>* rest
