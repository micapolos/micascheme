import leo scheme

check equal?
  let
    hello "Hello"
    world "world"
    in string-append: hello, ", ", world, "!"
  "Hello, world!"

check equal?
  let sequential
    hello "Hello"
    world "world"
    hello-world string-append: hello, ", ", world
    in string-append: hello-world, "!"
  "Hello, world!"

check equal?
  let
    with
      values: hello, world
      values: "Hello", "world"
    with
      values: comma, exclamation
      values: ", ", "!"
    in string-append: hello, comma, world, exclamation
  "Hello, world!"

check equal?
  let sequential
    with
      values: hello, world
      values: "Hello", "world"
    with
      values hello-world
      string-append: hello, ", ", world
    in string-append: hello-world, "!"
  "Hello, world!"

check equal?
  let fibonacci
    number 10
    in if
      less?: number, 2
      number
      add
        fibonacci subtract: number, 1
        fibonacci subtract: number, 2
  55
