import
  leo scheme
  leo document

document readme
  leo scheme programming language

  leo scheme is a dialect of scheme which uses indentation instead of parentheses
  this document is a valid program in leo scheme
  it can be executed in command line with "leo readme.leo"

  quick start
    overview
      leo translates indentation into standard scheme s-expressions
      each indented block represents a new level of nesting
      `example` form contain code snippets to execute

    definitions` example
      define hello "Hello"
      define leo "Leo"

      define
        exclamated s
        string-append: s, "!"

      define
        comma-separated: a, b
        string-append: a, ", ", b

      exclamated comma-separated: hello, leo

    control flow` example
      if
        greater?: 10, 5
        "Obviously, 10 is greater than 5."
        "What? 10 is not greater than 5?"

    lists` example
      define numbers list: 1, 2, 3
      define fruits list: 'apple, 'banana, 'orange
      define various list: true, char a, and numbers

      `lists
        numbers` numbers
        fruits` fruits
        various` various
        numbers and fruits` append: numbers, fruits
        strings` map: number->string, numbers
        odd numbers` filter: odd?, numbers
        sum` fold-left: add, 0, numbers

    quoting`
      example 'this is a-quote
      example `this is a-quasiquote
      example `this is a-quasiquote with an-unquote` add: 2, 2
      example `this is a-quasiquote with an-unquote-splicing`... list: 1, 2, 3, 4
      example `circle
        radius` sqrt 4
        center point
          x` sin 10
          y` cos 10

    define syntax` example
      define syntax
        when
          magic s
          string-append: "Hello, ", s, "!"
        when
          magic: a, b
          add: a, b
        when
          magic: a, ...
          list: 'a, ...

      `examples
        greeting` magic "world"
        addition` magic: 10, 20
        quoted list` magic: apple, orange, banana
