import leo scheme

define
  to-string x
  switch
    x
    when
      string? s
      string-append: "string: ", s
    when
      number? n
      string-append
        "number: "
        number->string n
    when
      any? x
      string-append
        "something else: "
        format: "~s", x

display to-string "jajko"
the newline

display to-string 123
the newline

display to-string char space
the newline
