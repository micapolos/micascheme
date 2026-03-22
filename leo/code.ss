(library (leo code)
  (export
    line-code

    atom-code?
    limited-simple-string?)
  (import
    (micascheme)
    (leo datum)
    (code))

  (define comma-code (code ", "))
  (define dot-code (code " . "))
  (define null-line-code (code "#null"))

  (define (boolean-line-code $boolean)
    (string-code (if $boolean "#true" "#false")))

  (define (number-line-code $number)
    (string-code (number->string $number)))

  (define (char-line-code $char)
    (string-code
      (string-append "#char "
        (lets
          ($string (format "~s" $char))
          (substring $string 2 (string-length $string))))))

  (define (string-line-code $string)
    (string-code (format "~s" $string)))

  (define (symbol-line-code $symbol)
    (string-code (format "~s" $symbol)))

  (define (pair-line-code $pair)
    (lets
      ($car-code (line-code (car $pair)))
      (switch (cdr $pair)
        ((null? _)
          (code-in-round-brackets $car-code))
        ((pair? $cdr-pair)
          (switch (cdr $cdr-pair)
            ((null? _)
              (space-separated-code $car-code (line-code (car $cdr-pair))))
            ((else $cdr-cdr)
              (space-separated-code $car-code
                (code-in-round-brackets (lines-code $cdr-pair))))))
        ((else $cdr)
          (code $car-code dot-code (line-code $cdr))))))

  (define (lines-code $lines)
    (switch-exhaustive $lines
      ((null? _) (code))
      ((pair? $pair)
        (lets
          ($car-code (line-code (car $pair)))
          (switch (cdr $pair)
            ((null? _)
              $car-code)
            ((pair? $cdr)
              (code $car-code comma-code (lines-code $cdr)))
            ((else $cdr)
              (code $car-code dot-code (line-code $cdr))))))))

  (define (bytevector-line-code $bytevector)
    (space-separated-code "#bytevector"
      (code-in-round-brackets
        (lines-code
          (bytevector->u8-list $bytevector)))))

  (define (vector-line-code $vector)
    (space-separated-code "#vector"
      (code-in-round-brackets
        (lines-code
          (vector->list $vector)))))

  (define (box-line-code $box)
    (space-separated-code "#box"
      (line-code
        (unbox $box))))

  (define (line-code $datum)
    (switch $datum
      ((null? _) null-line-code)
      ((boolean? $boolean) (boolean-line-code $boolean))
      ((number? $number) (number-line-code $number))
      ((char? $char) (char-line-code $char))
      ((string? $string) (string-line-code $string))
      ((symbol? $symbol) (symbol-line-code $symbol))
      ((pair? $pair) (pair-line-code $pair))
      ((box? $box) (box-line-code $box))
      ((bytevector? $bytevector) (bytevector-line-code $bytevector))
      ((vector? $vector) (vector-line-code $vector))
      ((else $other) (string-code (format "~s" $other)))))

  (define (atom-code? $datum)
    (switch $datum
      ((pair? $pair) #f)
      ((else $other) (string-code (format "~s" $other)))))

  (define (atom-code?-limiter $datum)
    (switch (atom-code? $datum)
      ((false? _) (limiter #f))
      ((else $code) (limiter-using $code 1))))

  (define (simple-code?-limiter $datum)
    (limiter-switch (atom-code?-limiter $datum)
      ((false? _)
        (switch $datum
          ((pair? $pair)
            (limiter-lets?
              ($car-code (atom-code?-limiter (car $pair)))
              (switch (cdr $pair)
                ((null? _)
                  (limiter (code "(" $car-code ")")))
                ((pair? $cdr)
                  (limiter-lets?
                    ($cdr-code (simple-code?-limiter $cdr))
                    (limiter (code $car-code " " $cdr-code))))
                ((else $cdr)
                  (limiter-lets?
                    ($cdr-code (atom-code?-limiter $cdr))
                    (limiter (code $car-code " . " $cdr-code)))))))
          ((else _)
            (limiter #f))))
      ((else $atom-code)
        (limiter $atom-code))))

  (define (limited-simple-code? $datum $limit)
    (limiter-apply (simple-code?-limiter $datum) $limit))

  (define (limited-simple-string? $datum $limit)
    (lets?
      ($limited (limited-simple-code? $datum $limit))
      (limited-map $limited code-string)))
)
