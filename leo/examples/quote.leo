import
  leo scheme
  only: datum->string, from micascheme

pretty-print quote string-append: "foo", "bar"
pretty-print << this is quoted >> string-append: "foo", "bar"
pretty-print << this is quoted list >>... list: "foo", "bar"
