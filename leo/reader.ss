(library (leo reader)
  (export
    reader reader? reader-append-fn reader-begin-fn reader-end-fn
    reader-append reader-begin reader-end
    reader-read reader-read-list
    reader-eval
    define-reader

    fold-reader
    reader-map
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

  (define (reader-map $fn $reader)
    (reader
      (lambda ($literal)
        (reader-map $fn
          (reader-append $reader $literal)))
      (lambda ($symbol)
        (reader-map $fn
          (reader-begin $reader $symbol)))
      (lambda ()
        (switch (reader-end $reader)
          ((reader? $reader)
            (reader-map $fn $reader))
          ((else $other)
            ($fn $other))))))

  (define-syntax-rule (define-reader ($name $arg ... $end) $body)
    (define $name
      (case-lambda
        (($arg ...) ($name $arg ... identity))
        (($arg ... $end) $body))))

  (define-reader (fold-reader $folded $append $child-reader $end)
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

  (define-reader (list-reader $end)
    (fold-reader (stack) push
      (lambda ($symbol $end)
        (list-reader
          (lambda ($list)
            ($end
              (if (null? $list)
                $symbol
                (cons $symbol $list))))))
      (lambda ($stack)
        ($end (reverse $stack)))))
)
