(library (leo-print)
  (export
    leo-print
    leo-line-print
    leo-line-println
    print-leo
    print-leo-line
    println-leo-line)
  (import (scheme) (switch) (procedure) (literal))

  (define-syntax print-leo
    (syntax-rules ()
      ((_ x ...) (leo-print '(x ...)))))

  (define-syntax print-leo-line
    (syntax-rules ()
      ((_ x) (indent/leo-print-line 0 'x))))

  (define-syntax println-leo-line
    (syntax-rules ()
      ((_ x) (begin (print-leo-line x) (newline)))))

  (define (leo-print $obj)
    (when (not (null? $obj))
      (indent/leo-print 0 $obj)
      (newline)))

  (define (leo-line-print $leo-line)
    (indent/leo-print-line 0 $leo-line))

  (define (leo-line-println $leo-line)
    (begin
      (indent/leo-print-line 0 $leo-line)
      (newline)))

  (define (indent/leo-print $indent $obj)
    (begin
      (switch $obj
        ((null? _) (void))
        ((pair? $pair)
          (begin
            (print-indent $indent)
            (indent/leo-print-line $indent (car $pair))
            (when (not (null? (cdr $pair)))
              (newline)
              (indent/leo-print $indent (cdr $pair)))))
        ((else $other)
          (begin
            (print-indent $indent)
            (display-string ". ")
            (write $other))))))

  (define (indent/leo-print-line $indent $line)
    (switch (line->leo-line? $line)
      ((pair? $pair)
        (begin
          (case (car $pair)
            ((quote)
              (display-string "{")
              (indent/leo-print $indent (cdr $pair))
              (display-string "}"))
            ((quasiquote)
              (display-string "{")
              (indent/leo-print $indent (cdr $pair)))
            ((unquote)
              (display-string "}")
              (indent/leo-print $indent (cdr $pair)))
            ((unquote-splicing)
              (display-string "}@")
              (indent/leo-print $indent (cdr $pair)))
            ((syntax)
              (display-string "{{")
              (indent/leo-print $indent (cdr $pair))
              (display-string "}}"))
            ((quasisyntax)
              (display-string "{{")
              (indent/leo-print $indent (cdr $pair)))
            ((unsyntax)
              (display-string "}}")
              (indent/leo-print $indent (cdr $pair)))
            ((unsyntax-splicing)
              (display-string "}}@")
              (indent/leo-print $indent (cdr $pair)))
            (else
              (write (car $pair))
              (switch (cdr $pair)
                ((null? _) (newline))
                ((single-line? $single-line)
                  (begin
                    (display-string " ")
                    (indent/leo-print-line $indent (car $single-line))))
                ((else $other)
                  (begin
                    (newline)
                    (indent/leo-print (+ $indent 1) $other))))))))
      ((else _)
        (switch $line
          ((boolean? $boolean)
            (write (if $boolean 'true 'false)))
          ((char? $char)
            (display-string
              (string-append
                "'"
                (list->string (cdr (cdr (string->list (format "~s" $char)))))
                "'")))
          ((else $other)
            (write $other))))))

  (define (print-indent $indent)
    (repeat $indent (display-string "  ")))

  (define (single-line? $obj)
    (and (pair? $obj) (null? (cdr $obj)) (car $obj)))

  (define (line->leo-line? $line)
    (switch? $line
      ((pair? $pair)
        (and (null? (car $pair))
          (switch? (cdr $pair)
            ((pair? $cdr-pair)
              (switch? (car $cdr-pair)
                ((symbol? $symbol) $cdr-pair))))))))
)
