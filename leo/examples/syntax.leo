import
  leo scheme
  only: datum->string, from micascheme

define remaining list
  syntax "goo"
  syntax "gar"
  syntax "goo"

displayln datum->string syntax->datum
  syntax string-append: "foo", "bar"

displayln datum->string syntax->datum
  <<< string-append: >>> "foo", >>> "bar"

displayln datum->string syntax->datum
  <<< string-append: >>> "foo", >>> "bar", >>>... remaining
