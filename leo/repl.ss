(library (leo repl)
  (export leo-repl)
  (import
    (except (micascheme) write)
    (only (leo read) leo-read)
    (leo exception-handler)
    (leo condition)
    (only (leo write) write))

  (define (leo-repl)
    (run
      (eval '(import (leo scheme)))
      (loop)))

  (define (clear-input-line port)
    (let ([c (read-char port)])
      (unless (or (eof-object? c) (char=? c #\newline))
        (clear-input-line port))))

  (define (loop)
    (run
      (display "> ")
      (flush-output-port)
      (guard
        ($condition
          (else
            (clear-input-line (console-input-port))
            (write-condition $condition)
            (loop)))
        (switch (leo-read (console-input-port))
          ((eof? _) (void))
          ((else $datum)
            (let (($evaled (eval $datum)))
              (write $evaled (console-output-port))
              (loop)))))))
)
