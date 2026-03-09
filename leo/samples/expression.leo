begin
  define
    exclamate s
    string-append
      s
      "!"
  string-append
    "Hello, "
    exclamate "world"
    " ("
    number->string string-length "foo"
    ")"
