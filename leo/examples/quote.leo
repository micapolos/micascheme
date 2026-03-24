import
  leo scheme
  only: datum->string, from micascheme

write quote string-append: "foo", "bar"
write << this is quoted >> string-append: "foo", "bar"
write << this is quoted list >>... list: "foo", "bar"
