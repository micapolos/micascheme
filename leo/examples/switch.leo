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

write to-string "jajko"
write to-string 123
write to-string char space
