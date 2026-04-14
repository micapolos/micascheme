import leo scheme

print let
  hello "Hello"
  leo "Leo"
  in string-append: hello, ", ", leo, "!"

print let*
  hello "Hello"
  leo "Leo"
  hello-leo string-append: hello, ", ", leo
  in string-append: hello-leo, "!"

print let-values
  with
    values: hello, leo
    values: "hello", "Leo"
  with
    values: comma, exclamation
    values: ", ", "!"
  in string-append: hello, comma, leo, exclamation

print let*-values
  with
    values: hello, leo
    values: "hello", "Leo"
  with
    values hello-leo
    string-append: hello, ", ", leo
  in string-append: hello-leo, "!"
