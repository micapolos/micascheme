import leo scheme

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

displayln to-string string-append: "Hello, ", "world!"
displayln to-string add: 2, 2
displayln to-string cons: 10, 20
