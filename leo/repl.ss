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

  (define (loop)
    (with-exception-handler
      (lambda ($condition)
        (write-condition $condition)
        (loop))
      (lambda ()
        (let loop ()
          (display "> " (console-output-port))
          (switch (leo-read (console-input-port))
            ((eof? _) (void))
            ((else $datum)
              (let (($evaled (eval $datum)))
                (write $evaled (console-output-port))
                (loop))))))))
)
