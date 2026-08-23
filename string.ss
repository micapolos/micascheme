(library (string)
  (export
    empty-string
    string-empty?
    lines-string
    lines-string0
    string->ascii
    datum->string
    string-split)
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

  ;; SRFI 152 string-split implementation
  (define (string-split str delim)
    (let ([str-len (string-length str)]
          [delim-len (string-length delim)])
      (cond
        ;; Empty delimiter: split into individual single-character strings
        [(= delim-len 0)
         (map string (string->list str))]

        ;; String shorter than delimiter: return original string in a list
        [(< str-len delim-len)
         (list str)]

        ;; Standard case: search and slice
        [else
         (let loop ([i 0]
                    [start 0]
                    [acc '()])
           (cond
             ;; Past the end: grab final substring and reverse accumulator
             [(> i (- str-len delim-len))
              (reverse (cons (substring str start str-len) acc))]

             ;; Substring match found at index i
             [(string=? (substring str i (+ i delim-len)) delim)
              (loop (+ i delim-len)
                    (+ i delim-len)
                    (cons (substring str start i) acc))]

             ;; No match at index i: advance index
             [else
              (loop (+ i 1) start acc)]))])))
)
