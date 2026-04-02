import leo scheme

write
  let
    hello "Hello"
    leo "Leo"
    in string-append: hello, ", ", leo, "!"

write
  let sequential
    hello "Hello"
    leo "Leo"
    hello-leo string-append: hello, ", ", leo
    in string-append: hello-leo, "!"

write
  let
    with
      values: hello, leo
      values: "hello", "Leo"
    with
      values: comma, exclamation
      values: ", ", "!"
    in string-append: hello, comma, leo, exclamation

write
  let sequential
    with
      values: hello, leo
      values: "hello", "Leo"
    with
      values hello-leo
      string-append: hello, ", ", leo
    in string-append: hello-leo, "!"
