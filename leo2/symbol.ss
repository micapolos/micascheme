(library (leo2 symbol)
  (export
    depth->symbol
    symbol->index?)
  (import (leo2 base))

  (define (depth->symbol $depth)
    (string->symbol (format "v~a" $depth)))

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

)

