import
  except: let, if, from scheme
  leo let
  leo in
  leo with
  leo check
  leo recursive
  leo sequential
  leo if

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
      <: number, 2
      number
      +
        fibonacci -: number, 1
        fibonacci -: number, 2
  55
