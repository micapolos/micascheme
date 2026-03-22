import
  leo scheme
  only: datum->string, from micascheme

define remaining list
  syntax "goo"
  syntax "gar"
  syntax "goo"

pretty-print syntax string-append: "foo", "bar"
pretty-print <<< this is quoted >>> string-append: "foo", "bar"
pretty-print <<< this is quoted list >>>... list: "foo", "bar"
