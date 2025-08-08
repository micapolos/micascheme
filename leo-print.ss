(library (leo-print)
  (export leo-print print-leo)
  (import (scheme) (switch) (procedure) (literal))

  (define-syntax print-leo
    (syntax-rules ()
      ((_ x ...) (leo-print '(x ...)))))

  (define (leo-print $obj)
    (when (not (null? $obj))
      (indent/leo-print 0 $obj)
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
    (begin
      (switch $line
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
              ((scheme)
                (write (cdr $pair)))
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
        ((char? $char)
          (begin
            (display-string "'")
            (display-string (list->string (cdr (cdr (string->list (format "~s" $char))))))
            (display-string "'")))
        ((boolean? $boolean)
          (if $boolean
            (display-string "true")
            (display-string "false")))
        ((else $other)
          (write $other)))))

  (define (print-indent $indent)
    (repeat $indent (display-string "  ")))

  (define (single-line? $obj)
    (and (pair? $obj) (null? (cdr $obj)) (car $obj)))
)
