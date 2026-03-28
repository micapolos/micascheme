import leo scheme

write
  let
    with
      hello "Hello"
      world "world"
    string-append: hello, ", ", world, "!"

write
  let*
    with
      hello "Hello"
      world "world"
      hello-world string-append: hello, ", ", world
    string-append: hello-world, "!"

write
  let-values
    with
      with
        values: hello, world
        values: "hello", "world"
      with
        values: comma, exclamation
        values: ", ", "!"
    string-append: hello, comma, world, exclamation

write
  let*-values
    with
      with
        values: hello, world
        values: "hello", "world"
      with
        values hello-world
        string-append: hello, ", ", world
    string-append: hello-world, "!"
