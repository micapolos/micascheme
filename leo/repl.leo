library
  leo repl
  export repl
  import
    ansi-string
    getter
    port
    leo scheme
    leo exception-handler
    leo condition
    leo version
    leo expand
    leo getter
    only: leo-read-annotation, from leo read
    only: eof?, char-newline?, from micascheme

  define
    repl:
    with-exception-handler
      leo-exception-handler
      lambda
        display-line string-append: "Leo Scheme ", version
        newline:
        eval 'import leo scheme
        loop:

  define
    clear-input-line port
    when
      char-ready? port
      let
        peeked-char peek-char port
        in unless
          or
            eof-object? peeked-char
            char=?: peeked-char, char newline
          read-char port
          clear-input-line port

  define
    char/eof-newline? char/eof
    and
      not eof? char/eof
      char-newline? char/eof

  define
    loop:
    display string-append: ansi-purple-string, ">>>", ansi-black-string, " "
    flush-output-port:
    guard
      raised-condition
        else
          let
            prefixed-port
              make-prefixed-textual-output-port
                console-output-port:
                string-append: ansi-cyan-string, "<<<", ansi-bright-red-string, " "
            in
              clear-input-line console-input-port:
              flush-output-port:
              display ansi-bright-red-string
              write-condition: raised-condition, prefixed-port
              flush-output-port prefixed-port
              display ansi-black-string
              loop:
      switch
        peek-char console-input-port:
        when
          char/eof-newline? _
          run
            get-char console-input-port:
            loop:
        when
          any? _
          let
            with
              values: leo-annotation/eof, _
              leo-read-annotation
                make-prefixed-textual-input-port
                  console-input-port:
                  string-append: ansi-purple-string, "...", ansi-black-string, " "
                  console-output-port:
                source-file-descriptor: "repl", 0
                0
            in switch
              leo-annotation/eof
              when
                eof? _
                newline:
              when
                any? leo-annotation
                let
                  evaluated eval leo-annotation
                  prefixed-port
                    make-prefixed-textual-output-port
                      console-output-port:
                      string-append: ansi-cyan-string, "<<<", ansi-black-string, " "
                  in
                    flush-output-port:
                    unless
                      equal?
                        evaluated
                        void:
                      write: evaluated, prefixed-port
                      flush-output-port prefixed-port
                      loop:
