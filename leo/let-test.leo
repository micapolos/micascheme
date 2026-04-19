import
  only
    equal?, string-append, =, <, +, -, values
    from scheme
  leo let
  leo with
  leo check
  leo if
  leo keywords

check equal?
  let
    with
      hello "Hello"
      world "world"
    string-append: hello, ", ", world, "!"
  "Hello, world!"

check equal?
  let
    hello "Hello"
    world "world"
    in string-append: hello, ", ", world, "!"
  "Hello, world!"

check equal?
  let*
    hello "Hello"
    world "world"
    hello-world string-append: hello, ", ", world
    in string-append: hello-world, "!"
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

check equal?
  let fibonacci
    with number 10
    if
      <: number, 2
      number
      +
        fibonacci -: number, 1
        fibonacci -: number, 2
  55

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
