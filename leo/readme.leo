import leo scheme

pretty-print << readme
  leo scheme programming language

  programming language which uses indentation instead of parentheses
  this document is a valid program in leo scheme
  it uses quoting to separate prose from executable logic

  quick start
    overview
      leo translates indentation into standard scheme s-expressions
      each indented block represents a new level of nesting

    basic syntax >> run
      define hello "Hello"
      define world "world"
      string-append: hello, ", ", world, "!"

    functions >> run
      define
        exclamated s
        string-append: s, "!!!"
      << messages >>
        exclamated "Cool"
        exclamated "LOL"
        exclamated "Wow"

    control flow >> run
      if
        >: 10, 5
        quote 10 is greater than 5
        quote 10 is not greater than 5

    lists and maps >> run
      define numbers list: 1, 2, 3, 4, 5, 6
      << examples
        numbers >> numbers
        strings >> map: number->string, numbers
        odd numbers? >> filter: odd?, numbers

    lambdas
      doubled >> map
        lambda
          with x
          *: x, 2
        list: 1, 2, 3, 4

      listed >>
        map
          lambda
            with: n, s
            string-append: number->string n, ": ", s
          list: 1, 2, 3, 4
          list: "apple", "orange", "pear", "plum"

    macros >> run
      define-macro
        when
          my-macro s
          string-append: "Hello, ", s, "!"
        when
          my-macro: a, b
          +: a, b

      << examples >>
        my-macro "world"
        my-macro: 10, 20
