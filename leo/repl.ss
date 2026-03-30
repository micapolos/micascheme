(library (leo repl)
  (export leo-repl)
  (import
    (except (micascheme) write)
    (only (leo read) leo-read)
    (leo exception-handler)
    (leo condition)
    (leo version)
    (getter)
    (port)
    (leo getter)
    (only (leo write) write))

  (define (leo-repl)
    (run
      (displayln (string-append "Leo Scheme Version " version))
      (newline)
      (eval '(import (leo scheme)))
      (loop)))

  (define (clear-input-line port)
    (when (char-ready? port)
      (let ((c (peek-char port)))
        (unless (or (eof-object? c) (char=? c #\newline))
          (read-char port) ; Consume the character we just peeked at!
          (clear-input-line port)))))

  (define (loop)
    (run
      (display "\x1b;[35m>>>\x1b;[0m ")
      (flush-output-port)
      (guard
        ($condition
          (else
            (let
              (
                ($prefixed-port
                  (make-prefixed-textual-output-port
                    (console-output-port)
                    "\x1b;[36m<<<\x1b;[91m ")))
              (clear-input-line (console-input-port))
              (flush-output-port)
              (display "\x1b;[91m")
              (write-condition $condition $prefixed-port)
              (flush-output-port $prefixed-port))
              (display "\x1b;[0m")
              (loop)))
        (switch (peek-char (console-input-port))
          (((and? (not? eof?) char-newline?) _)
            (run
              (get-char (console-input-port))
              (loop)))
          ((else _)
            (switch
              (leo-read
                (make-prefixed-textual-input-port
                  (console-input-port)
                  "\x1b;[35m...\x1b;[0m "
                  (console-output-port)))
              ((eof? _)
                (newline))
              ((else $datum)
                (let
                  (
                    ($evaled (eval $datum))
                    ($prefixed-port
                      (make-prefixed-textual-output-port
                        (console-output-port)
                        "\x1b;[36m<<<\x1b;[0m ")))
                  (flush-output-port)
                  (unless (equal? $evaled (void))
                    (write $evaled $prefixed-port)
                    (flush-output-port $prefixed-port))
                  (loop)))))))))
)
