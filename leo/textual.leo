library
  leo textual
  export
    rename %textual textual
    textual?
    textual-current-put
    current-textual-string
    list->textual
    textual-append
  import
    leo scheme
    leo current
    data

  data textual current-put

  define
    syntax
      keywords: with, char
      when
        %textual
          with port
          x, xs, ...
        textual current lambda
          with port
          x, xs, ...

      when
        %textual char: x, xs, ...
        %textual
          with port
          put-char
            port
            char: x, xs, ...

      when
        %textual x
        string? datum x
        %textual
          with port
          put-string: port, x

    lambda
      current-textual-string: textual
      current-lets
        put textual-current-put textual
        current call-with-string-output-port put

    lambda
      list->textual textual-list
      %textual
        with port
        for-each
          lambda
            with current-put
            with: current-ref current-put, port
          map: textual-current-put, textual-list

    lambda
      textual-append and textual-list
      list->textual textual-list
