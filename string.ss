(library (string)
  (export
    empty-string
    string-empty?
    lines-string
    lines-string0
    string->ascii
    datum->string)
  (import (scheme) (list) (list-syntax) (procedure) (char))

  (define (empty-string) "")

  (define (string-empty? $string)
    (= (string-length $string) 0))

  (define (lines-string . $lines)
    (apply string-append
      (map-with ($line $lines)
        (string-append $line "\n"))))

  (define (lines-string0 . $lines)
    (apply string-append
      (intercalate $lines "\n")))

  (define (string->ascii $string)
    (u8-list->bytevector (map char->ascii (string->list $string))))

  (define (datum->string $datum)
    (format "~s" $datum))
)
