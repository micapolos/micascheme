import leo scheme

write
  let
    with
      hello "Hello"
      leo "Leo"
    string-append: hello, ", ", leo, "!"

write
  let*
    with
      hello "Hello"
      leo "Leo"
      hello-leo string-append: hello, ", ", leo
    string-append: hello-leo, "!"

write
  let-values
    with
      with
        values: hello, leo
        values: "hello", "Leo"
      with
        values: comma, exclamation
        values: ", ", "!"
    string-append: hello, comma, leo, exclamation

write
  let*-values
    with
      with
        values: hello, leo
        values: "hello", "Leo"
      with
        values hello-leo
        string-append: hello, ", ", leo
    string-append: hello-leo, "!"
