(library (leo code)
  (export
    line-code

    limit-simple-line-code?

    check-simple-line-code
    check-simple-line-code-false?)
  (import
    (micascheme)
    (leo datum)
    (code))

  (define comma-code (code ", "))
  (define dot-code (code " . "))

  (define (dot-separated-code $car $cdr)
    (code $car dot-code $cdr))

  ; === line-code ===

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

  (define (other-line-code $other)
    (string-code (format "~s" $other)))

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
      ((else $other) (other-line-code $other))))

  ; === line-code-limiter ===

  (define null-simple-line-code-limiter
    (limiter-using null-line-code 1))

  (define (boolean-simple-line-code-limiter $boolean)
    (limiter-using (boolean-line-code $boolean) 1))

  (define (number-simple-line-code-limiter $number)
    (limiter-using (number-line-code $number) 1))

  (define (char-simple-line-code-limiter $char)
    (limiter-using (char-line-code $char) 1))

  (define (string-simple-line-code-limiter $string)
    (limiter-using (string-line-code $string) 1))

  (define (symbol-simple-line-code-limiter $symbol)
    (limiter-using (symbol-line-code $symbol) 1))

  (define (bytevector-simple-line-code?-limiter $bytevector)
    (case (bytevector-length $bytevector)
      ((1)
        (limiter-lets?
          ($bytevector-code (limiter-using (code "#bytevector") 1))
          ($byte-code (simple-line-code?-limiter (bytevector-u8-ref $bytevector 0)))
          (limiter (space-separated-code $bytevector-code $byte-code))))
      (else (limiter #f))))

  (define (vector-simple-line-code?-limiter $vector)
    (case (vector-length $vector)
      ((1)
        (limiter-lets?
          ($vector-code (limiter-using (code "#vector") 1))
          ($ref-code (simple-line-code?-limiter (vector-ref $vector 0)))
          (limiter (space-separated-code $vector-code $ref-code))))
      (else (limiter #f))))

  (define (box-simple-line-code?-limiter $box)
    (limiter-lets?
      ($box-code (limiter-using (code "#box") 1))
      ($ref-code (simple-line-code?-limiter (unbox $box)))
      (limiter (space-separated-code $box-code $ref-code))))

  (define (pair-simple-line-code?-limiter $pair)
    (limiter-lets?
      ($car-code (simple-line-code?-limiter (car $pair)))
      (switch (cdr $pair)
        ((null? _)
          (limiter #f))
        ((pair? $cdr)
          (switch (cdr $cdr)
            ((null? _)
              (limiter-lets?
                ($cdr-code (simple-line-code?-limiter (car $cdr)))
                (limiter (space-separated-code $car-code $cdr-code))))
            ((else _)
              (limiter #f))))
        ((else $cdr)
          (limiter-lets?
            ($cdr-code (simple-line-code?-limiter $cdr))
            (limiter (dot-separated-code $car-code $cdr-code)))))))

  (define (other-simple-line-code-limiter $other)
    (limiter-using (other-line-code $other) 1))

  (define (simple-line-code?-limiter $datum)
    (switch $datum
      ((null? _) null-simple-line-code-limiter)
      ((boolean? $boolean) (boolean-simple-line-code-limiter $boolean))
      ((number? $number) (number-simple-line-code-limiter $number))
      ((char? $char) (char-simple-line-code-limiter $char))
      ((string? $string) (string-simple-line-code-limiter $string))
      ((symbol? $symbol) (symbol-simple-line-code-limiter $symbol))
      ((pair? $pair) (pair-simple-line-code?-limiter $pair))
      ((box? $box) (box-simple-line-code?-limiter $box))
      ((bytevector? $bytevector) (bytevector-simple-line-code?-limiter $bytevector))
      ((vector? $vector) (vector-simple-line-code?-limiter $vector))
      ((else $other) (other-simple-line-code-limiter $other))))

  (define (limit-simple-line-code? $limit $datum)
    (lets?
      ($limited (limiter-apply (simple-line-code?-limiter $datum) $limit))
      (limited-ref $limited)))

  (define-rule-syntax (check-simple-line-code size in out)
    (begin
      (check-code=? (limit-simple-line-code? size in) out)
      (check (false? (limit-simple-line-code? (- size 1) in)))))

  (define-rule-syntax (check-simple-line-code-false? size in)
    (check (false? (limit-simple-line-code? size in))))
)
