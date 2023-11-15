(library (writer)
  (export
    writer writer? writer-value writer-write-char-proc
    writer-write-char
    writer-write-string
    chars-writer
    indented-writer
    trim-end-writer
    do-writer-string)
  (import (micascheme))

  (data (writer value write-char-proc))

  (define (writer-write-char $writer $char)
    (app (writer-write-char-proc $writer) $char))

  (define (writer-write-string $writer $string)
    (fold-left writer-write-char $writer (string->list $string)))

  (define-syntax-rule (do-writer-string $writer $body)
    (list->string (reverse (writer-value (lets ($writer (chars-writer)) $body)))))

  (define chars-writer
    (case-lambda
      (() (chars-writer (stack)))
      (($chars)
        (writer $chars
          (lambda ($char)
            (chars-writer (push $chars $char)))))))

  (define indented-writer
    (case-lambda
      (() (indented-writer 2))
      (($size) (indented-writer $size (chars-writer)))
      (($size $writer)
        (writer $writer
          (lambda ($char)
            (lets
              ($writer
                (writer-write-char $writer $char))
              ($writer
                (cond
                  ((char=? $char #\newline)
                    (fold-left
                      writer-write-char
                      $writer
                      (make-list $size #\space)))
                  (else $writer)))
              (indented-writer $size $writer)))))))

  (define trim-end-writer
    (case-lambda
      (() (trim-end-writer (chars-writer)))
      (($writer) (trim-end-writer $writer (stack)))
      (($writer $trimmed-whitespaces)
        (writer $writer
          (lambda ($char)
            (cond
              ((char=? $char #\newline)
                (trim-end-writer
                  (writer-write-char $writer $char)))
              ((char-whitespace? $char)
                (trim-end-writer
                  $writer
                  (push $trimmed-whitespaces $char)))
              (else
                (trim-end-writer
                  (writer-write-char
                    (fold-left
                      writer-write-char
                      $writer
                      (reverse $trimmed-whitespaces))
                    $char)))))))))
)
