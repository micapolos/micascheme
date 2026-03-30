library
  leo repl
  export leo-repl
  import
    leo scheme
    only: leo-read-annotation, from leo read
    leo exception-handler
    leo condition
    leo version
    leo expand
    getter
    port
    leo getter
    ansi-string
    only: eof?, char-newline?, from micascheme

  define
    leo-repl:
    with-exception-handler
      leo-exception-handler
      lambda
        with:
        display-line string-append: "Leo Scheme ", version
        newline:
        eval cons
          leo-expand
          'import leo scheme
        loop:

  define
    clear-input-line port
    when
      char-ready? port
      let
        c peek-char port
        in unless
          or
            eof-object? c
            char=?: c, char newline
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
      $condition
        else
          let
            $prefixed-port
              make-prefixed-textual-output-port
                console-output-port:
                string-append: ansi-cyan-string, "<<<", ansi-bright-red-string, " "
            in
              clear-input-line console-input-port:
              flush-output-port:
              display ansi-bright-red-string
              write-condition: $condition, $prefixed-port
              flush-output-port $prefixed-port
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
              values: $annotation/eof, _
              leo-read-annotation
                make-prefixed-textual-input-port
                  console-input-port:
                  string-append: ansi-purple-string, "...", ansi-black-string, " "
                  console-output-port:
                source-file-descriptor: "repl", 0
                0
            in switch
              $annotation/eof
              when
                eof? _
                newline:
              when
                any? $annotation
                let
                  $evaled eval cons
                    leo-expand
                    $annotation
                  $prefixed-port
                    make-prefixed-textual-output-port
                      console-output-port:
                      string-append: ansi-cyan-string, "<<<", ansi-black-string, " "
                  in
                    flush-output-port:
                    unless
                      equal?
                        $evaled
                        void:
                      write: $evaled, $prefixed-port
                      flush-output-port $prefixed-port
                      loop:
