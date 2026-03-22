```
leo scheme programming language

leo scheme is a programming language which uses indentation instead of parentheses
this document is a valid leo scheme program
it uses << and >> to separate prose from executable logic

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
    << exclamated strings >>
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
      number strings >> map: number->string, numbers
      odd numbers >> filter: odd?, numbers

  lambdas >>
    map
      lambda
        with x
        list:
          << original >> x
          << incremented >> +: x, 1
          << doubled >> *: x, 2
          << as string >> number->string x
      list: 1, 2, 3, 4

  macros >> run
    define-macro
      when
        my-macro s
        string-append: "Hello, ", s, "!"
      when
        my-macro: a, b
        +: a, b

    << examples
      greeting >> my-macro "world"
      addition >> my-macro: 10, 20
```
