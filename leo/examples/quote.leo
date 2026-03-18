import
  leo scheme
  only: datum->string, from micascheme

define remaining list
  "goo"
  "gar"
  "goo"

displayln datum->string
  quote string-append: "foo", "bar"

displayln datum->string
  << string-append: >> "foo", >> "bar"

displayln datum->string
  << string-append: >> "foo", >> "bar", >>... remaining
