leo scheme programming language

programming language which uses indentation instead of parentheses

quick start
  overview
    leo translates indentation into standard scheme s-expressions
    each indented block represents a new level of nesting

  basic syntax
    define hello "Hello"
    define world "world"

    displayln string-append: hello, ", ", world, "!"

  functions
    define
      exclamated s
      string-append: s, "!"

    displayln exclamated "He"

  control flow
    if
      >: 10, 5
      displayln "10 is greater"
      displayln "this won't happen"

  lists and maps
    define numbers list: 1, 2, 3, 4, 5, 6

    pretty-print numbers
    pretty-print map: number->string, numbers
    pretty-print filter: odd?, numbers

  lambdas
    map
      lambda
        with x
        *: x, 2
      list: 1, 2, 3, 4

    map
      lambda
        with: n, s
        string-append: number->string n, ": ", s
      list: 1, 2, 3, 4
      list: "apple", "orange", "pear", "plum"

  quoting
    pretty-print quote +: 10, 20
    pretty-print << a number >> +: 10, 20

  macros
    define-macro
      when
        my-macro s
        string-append: "Hello, ", s, "!"
      when
        my-macro: a, b
        +: a, b

    pretty-print my-macro "world"
    pretty-print my-macro: 10, 20

