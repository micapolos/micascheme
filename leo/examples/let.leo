import leo scheme

write
  let
    hello "Hello"
    world "world"
    in string-append: hello, ", ", world, "!"

write
  let-sequential
    hello "Hello"
    world "world"
    hello-world string-append: hello, ", ", world
    in string-append: hello-world, "!"

write
  let-values
    with
      values: hello, world
      values: "hello", "world"
    with
      values: comma, exclamation
      values: ", ", "!"
    in string-append: hello, comma, world, exclamation

write
  let-sequential-values
    with
      values: hello, world
      values: "hello", "world"
    with
      values hello-world
      string-append: hello, ", ", world
    in string-append: hello-world, "!"
