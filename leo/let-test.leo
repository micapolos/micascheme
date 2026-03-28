import leo scheme

check equal?
  let
    with
      hello "Hello"
      world "world"
    string-append: hello, ", ", world, "!"
  "Hello, world!"

check equal?
  let*
    with
      hello "Hello"
      world "world"
      hello-world string-append: hello, ", ", world
    string-append: hello-world, "!"
  "Hello, world!"

check equal?
  let-values
    with
      with
        values: hello, world
        values: "Hello", "world"
      with
        values: comma, exclamation
        values: ", ", "!"
    string-append: hello, comma, world, exclamation
  "Hello, world!"

check equal?
  let*-values
    with
      with
        values: hello, world
        values: "Hello", "world"
      with
        values hello-world
        string-append: hello, ", ", world
    string-append: hello-world, "!"
  "Hello, world!"
