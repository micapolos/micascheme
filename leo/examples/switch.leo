import leo scheme

define lambda
  to-string x
  switch x
    when
      string string?
      string-append: "string ", string
    when
      number number?
      string-append
        "number "
        number->string number
    else other
      string-append
        "other "
        format: "~s", other

display to-string string-append: "Hello, ", "world!"
display to-string add: 2, 2
display to-string cons: 10, 20
