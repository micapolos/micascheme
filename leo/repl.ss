(library (leo repl)
  (export leo-repl)
  (import
    (except (micascheme) write)
    (only (leo read) leo-read)
    (leo exception-handler)
    (leo condition)
    (leo version)
    (getter)
    (leo getter)
    (only (leo write) write))

  (define (leo-repl)
    (run
      (displayln (string-append "Leo Scheme Version " version))
      (newline)
      (eval '(import (leo scheme)))
      (parameterize ((leo-getter-empty-lines? #f))
        (loop))))

  (define (clear-input-line port)
    (let ([c (read-char port)])
      (unless (or (eof-object? c) (char=? c #\newline))
        (clear-input-line port))))

  (define (make-prefixed-output-port $port $prefix)
    (let ((new-line? #t))
      (define (maybe-write-prefix)
        (when new-line?
          (display $prefix $port)
          (set! new-line? #f)))

      (define (write! str start count)
        (let loop ((i start) (written 0))
          (if (< i (+ start count))
              (let ([char (string-ref str i)])
                (maybe-write-prefix)
                (write-char char $port)
                (when (char=? char #\newline)
                  (set! new-line? #t))
                (loop (+ i 1) (+ written 1)))
              written)))

      (make-custom-textual-output-port
        (string-append "prefixed-" (port-name $port))
        write!
        (lambda () (port-position $port))
        (lambda ($position) (set-port-position! $port $position))
        (lambda () (close-port $port)))))

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
        (switch (peek-char (console-input-port))
          (((and? (not? eof?) char-newline?) _)
            (run
              (get-char (console-input-port))
              (loop)))
          ((else _)
            (switch (parameterize ((getter-prompt ". ")) (leo-read (console-input-port)))
              ((eof? _)
                (newline))
              ((else $datum)
                (let (($evaled (eval $datum)))
                  (newline)
                  (write $evaled (console-output-port))
                  (loop)))))))))
)
