(library (writer)
  (export
    writer writer? writer-value writer-write-char-proc
    writer-write-char
    writer-write-string
    chars-writer
    indented-writer)
  (import (micascheme))

  (data (writer value write-char-proc))

  (define (writer-write-char $writer $char)
    (app (writer-write-char-proc $writer) $char))

  (define (writer-write-string $writer $string)
    (fold-left writer-write-char $writer (string->list $string)))

  (define chars-writer
    (case-lambda
      (() (chars-writer (stack)))
      (($chars)
        (writer $chars
          (lambda ($char)
            (chars-writer (push $chars $char)))))))

  (define (indented-writer $writer $size)
    (writer
      $writer
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
          (indented-writer $writer $size)))))
)
