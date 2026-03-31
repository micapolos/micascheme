library
  leo textual
  export
    textual
  import
    leo scheme
    monadic

  define
    syntax
      keywords: with
      when
        textual: with port, body
        lambda: with port, body
      when
        textual s
        string? datum s
        textual
          with port
          put-string: port, s
      when
        textual ch
        char? datum ch
        textual
          with port
          put-char: port, ch

    lambda
      list->textual textual-list

