(library
  (leo repl)
  (export repl)
  (import
    (except (scheme) write)
    (ansi-string)
    (char)
    (eof)
    (getter)
    (port)
    (void)
    (switch)
    (lets)
    (procedure)
    (leo exception-handler)
    (leo condition)
    (leo version)
    (leo write)
    (leo expand)
    (leo getter)
    (leo read))

  (define (repl)
    (with-exception-handler
      leo-exception-handler
      (lambda ()
        (display (string-append "Leo Scheme " version))
        (newline)
        (newline)
        (eval '(import (leo scheme)))
        (loop))))

  (define (read-until-newline port)
    (when (char-ready? port)
      (lets
        (peeked-char (peek-char port))
        (unless
          (or
            (eof-object? peeked-char)
            (char-newline? peeked-char))
          (read-char port)
          (read-until-newline port)))))

  (define (char/eof-newline? char/eof)
    (and
      (not (eof? char/eof))
      (char-newline? char/eof)))

  (define (loop)
    (display (string-append ansi-purple-string ">>>" ansi-black-string " "))
    (flush-output-port)
    (guard
      (raised-condition
        (else
          (lets
            (prefixed-port
              (make-prefixed-textual-output-port
                (console-output-port)
                (string-append ansi-cyan-string "<<<" ansi-bright-red-string " ")))
            (run
              (read-until-newline (console-input-port))
              (flush-output-port)
              (display ansi-bright-red-string)
              (write-condition raised-condition prefixed-port)
              (flush-output-port prefixed-port)
              (display ansi-black-string)
              (loop)))))
      (switch (peek-char (console-input-port))
        ((char/eof-newline? _)
          (run
            (get-char (console-input-port))
            (loop)))
        ((else _)
          (lets
            ((values leo-annotation/eof _)
              (leo-read-annotation
                (make-prefixed-textual-input-port
                  (console-input-port)
                  (string-append ansi-purple-string "..." ansi-black-string " ")
                  (console-output-port))
                (source-file-descriptor "repl" 0)
                0))
            (switch leo-annotation/eof
              ((eof? _) (newline))
              ((else leo-annotation)
                (lets
                  (evaluated (eval leo-annotation))
                  (prefixed-port
                    (make-prefixed-textual-output-port
                      (console-output-port)
                      (string-append ansi-cyan-string "<<<" ansi-black-string " ")))
                  (run
                    (flush-output-port)
                    (unless
                      (void? evaluated)
                      (write evaluated prefixed-port)
                      (flush-output-port prefixed-port))
                    (loop))))))))))
)
