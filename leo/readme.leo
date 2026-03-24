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
      text and code is separated using << and >> quotes
      << example >> form contain code snippets to execute

    basic syntax >> example
      define hello "Hello"
      define world "world"
      string-append: hello, ", ", world, "!"

    functions >> example
      define
        exclamated s
        string-append: s, "!!!"
      << exclamated strings >>
        exclamated "Cool"
        exclamated "LOL"
        exclamated "Wow"

    control flow >> example
      if
        greater?: 10, 5
        "Obviously, 10 is greater than 5."
        "What? 10 is not greater than 5?"

    lists and maps >> example
      define numbers list: 1, 2, 3, 4, 5, 6

      << lists
        numbers >>... numbers
        strings >>... map: number->string, numbers
        odd numbers >>... filter: odd?, numbers

    lambdas >> example
      << mapped >>... map
        lambda
          with x
          << mapping
            original >> x
            ncremented >> add: x, 1
            doubled >> multiply: x, 2
            to string >> number->string x
        list: 1, 2, 3, 4

    macros >> example
      define-macro
        when
          my-macro s
          string-append: "Hello, ", s, "!"
        when
          my-macro: a, b
          add: a, b
        when
          my-macro: a, ...
          list: quote a, ...

      << examples
        greeting >> my-macro "world"
        addition >> my-macro: 10, 20
        quoted list >> my-macro: apple, orange, banana, pear, ...
