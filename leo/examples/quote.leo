import
  leo scheme
  only: datum->string, from micascheme

write quote string-append: "foo", "bar"

write << this is quoted >> string-append: "foo", "bar"

write << this is quoted list >>... list: "foo", "bar"

write << this is structural quoting circle
  radius >> add: 10, 20
  center point
    x >> sin 10
    y >> cos 10

write quote << this is structural quoting circle
  radius >> add: 10, 20
  center point
    x >> sin 10
    y >> cos 10
