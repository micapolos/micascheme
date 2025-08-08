(library (leo-print)
  (export leo-print)
  (import (scheme) (switch) (procedure) (literal))

  (define (leo-print $obj)
    (indent/leo-print 0 $obj))

  (define (indent/leo-print $indent $obj)
    (begin
      (switch $obj
        ((null? _) (void))
        ((pair? $pair)
          (begin
            (print-indent $indent)
            (indent/leo-print-line $indent (car $pair))
            (indent/leo-print $indent (cdr $pair))))
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
            (write (car $pair))
            (switch (cdr $pair)
              ((null? _) (newline))
              ((single-line? $single-line)
                (begin
                  (display-string " ")
                  (indent/leo-print-line $indent $single-line)))
              ((else $other)
                (begin
                  (newline)
                  (indent/leo-print (+ $indent 1) $other))))))
        ((else $other)
          (begin
            (write $other)
            (newline))))))

  (define (print-indent $indent)
    (repeat $indent (display-string "  ")))

  (define (single-line? $obj)
    (and (pair? $obj) (null? (cdr $obj)) (car $obj)))
)
