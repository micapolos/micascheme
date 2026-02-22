(library (leo2 lang symbol)
  (export
    symbol->index?
    index->symbol)
  (import (leo2 base))

  (define (symbol->index? $symbol)
    (lets
      ($string (symbol->string $symbol))
      ($length (string-length $string))
      (and
        (> $length 0)
        (char=? (string-ref $string 0) #\$)
        (lets
          ($number (string->number (substring $string 1 (string-length $string))))
          (and
            (integer? $number)
            (positive? $number)
            (- $number 1))))))

  (define (index->symbol $index)
    (string->symbol
      (string-append "$"
        (number->string (+ $index 1)))))
)

