import leo scheme

define lambda
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

display-line to-string string-append: "Hello, ", "world!"
display-line to-string add: 2, 2
display-line to-string cons: 10, 20
