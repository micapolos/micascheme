import
  leo scheme
  only: datum->string, from micascheme

write 'quoted string-append: "foo", "bar"

write `quasiquoted and unquoted` string-append: "foo", "bar"

write `quasiquoted and unquoted-splicing`... list: "foo", "bar"

write `circle
  radius` add: 10, 20
  center point
    x` sin 10
    y` cos 10
  labels`... list: "nice", "looking", "circle"

write 'quoted `circle
  radius` add: 10, 20
  center point
    x` sin 10
    y` cos 10
  labels`... list: "nice", "looking", "circle"
