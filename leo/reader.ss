(library (leo reader)
  (export
    reader reader? reader-value reader-append-fn reader-begin-fn reader-end-fn
    reader-append reader-begin reader-end
    reader-read reader-read-list
    reader-eval

    list-reader)
  (import (micascheme))

  (data (reader value append-fn begin-fn end-fn))

  (define (reader-append $reader $value)
    ((reader-append-fn $reader) $value))

  (define (reader-begin $reader $symbol)
    ((reader-begin-fn $reader) $symbol))

  (define (reader-end $reader)
    ((reader-end-fn $reader) (reader-value $reader)))

  (define (reader-read-list $reader $list)
    (fold-left reader-read $reader $list))

  (define (reader-read $reader $datum)
    (switch $datum
      ((symbol? $symbol)
        (reader-end
          (reader-begin $reader $symbol)))
      ((pair? $pair)
        (reader-end
          (reader-read-list
            (reader-begin $reader (car $pair))
            (cdr $pair))))
      ((else $other)
        (reader-append $reader $other))))

  (define-syntax-rule (reader-eval $reader $item ...)
    (reader-end (reader-read-list $reader (list (quote $item) ...))))

  (define list-reader
    (case-lambda
      (()
        (list-reader identity))
      (($end-fn)
        (list-reader (stack) $end-fn))
      (($stack $end-fn)
        (reader $stack
          (lambda ($appended-item)
            (list-reader
              (push $stack $appended-item)
              $end-fn))
          (lambda ($begin-symbol)
            (list-reader
              (lambda ($list)
                (list-reader
                  (push $stack
                    (if (null? $list)
                      $begin-symbol
                      `(,$begin-symbol ,@$list)))
                  $end-fn))))
          (lambda ($end-stack)
            ($end-fn (reverse $end-stack)))))))
)