(library (leo reader)
  (export
    reader reader? reader-append-fn reader-begin-fn reader-end-fn
    reader-append reader-begin reader-end
    reader-read reader-read-list
    reader-eval

    fold-reader
    datums-reader
    list-reader)
  (import (micascheme))

  (data (reader append-fn begin-fn end-fn))

  (define (reader-append $reader $value)
    ((reader-append-fn $reader) $value))

  (define (reader-begin $reader $symbol)
    ((reader-begin-fn $reader) $symbol))

  (define (reader-end $reader)
    ((reader-end-fn $reader)))

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

  (define (fold-reader $folded $append $child-reader $end)
    (reader
      (lambda ($literal)
        (fold-reader
          ($append $folded $literal)
          $append
          $child-reader
          $end))
      (lambda ($symbol)
        ($child-reader $symbol
          (lambda ($child-item)
            (fold-reader
              ($append $folded $child-item)
              $append
              $child-reader
              $end))))
      (lambda ()
        ($end $folded))))

  (define (datums-reader $end)
    (fold-reader (stack) push
      (lambda ($symbol $end)
        (datums-reader
          (lambda ($list)
            ($end (cons $symbol $list)))))
      (lambda ($stack)
        ($end (reverse $stack)))))

  (define list-reader
    (case-lambda
      (()
        (list-reader identity))
      (($end-fn)
        (list-reader (stack) $end-fn))
      (($stack $end-fn)
        (reader
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
          (lambda ()
            ($end-fn
              (reverse $stack)))))))
)
