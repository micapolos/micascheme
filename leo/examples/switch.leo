import
  leo scheme
  math

define
  to-string x
  switch
    x
    when
      string? s
      string-append: "string ", s
    when
      number? n
      string-append
        "number "
        number->string n
    when
      any? x
      string-append
        "other "
        format: "~s", x

display to-string string-append: "Hello, ", "world!"
the newline

display to-string add: 2, 2
the newline

display to-string cons: 10, 20
the newline
