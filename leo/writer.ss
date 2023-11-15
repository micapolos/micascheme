(library (leo writer)
  (export
    datums-string)
  (import
    (micascheme)
    (writer))

  (define-syntax-rule (datums-string $item ...)
    (list->string
      (reverse
        (writer-value
          (writer-write-datums
            (chars-writer)
            '($item ...))))))

  (define (writer-write-datums $writer $datums)
    (fold-left
      (lambda ($writer $datum)
        (writer-write-char
          (writer-write-datum $writer $datum)
          #\newline))
      $writer
      $datums))

  (define (writer-write-newline-datums $writer $datums)
    (fold-left
      (lambda ($writer $datum)
        (writer-write-datum
          (writer-write-char $writer #\newline)
          $datum))
      $writer
      $datums))

  (define (writer-write-datum $writer $datum)
    (switch $datum
      ((symbol? $symbol)
        (writer-write-symbol $writer $symbol))
      ((pair? $pair)
        (unpair $pair $symbol $datums
          (lets
            ($writer (writer-write-symbol $writer $symbol))
            (case (length $datums)
              ((0) $writer)
              ((1)
                (writer-write-datum
                  (writer-write-char $writer #\space)
                  (car $datums)))
              (else
                (writer-value
                  (writer-write-newline-datums
                    (indented-writer $writer 2)
                    $datums)))))))
      ((else $other)
        (writer-write-string $writer
          (format "~s" $other)))))

  (define (writer-write-symbol $writer $symbol)
    (writer-write-string $writer (symbol->string $symbol)))
)
