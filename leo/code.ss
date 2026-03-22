(library (leo code)
  (export
    line-code

    check-space-line-code
    check-space-line-code-false?

    check-colon-line-code
    check-colon-line-code-false?)
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

  ; === space-line-code-limiter ===

  (define null-space-line-code-limiter
    (limiter-using null-line-code 1))

  (define (boolean-space-line-code-limiter $boolean)
    (limiter-using (boolean-line-code $boolean) 1))

  (define (number-space-line-code-limiter $number)
    (limiter-using (number-line-code $number) 1))

  (define (char-space-line-code-limiter $char)
    (limiter-using (char-line-code $char) 1))

  (define (string-space-line-code-limiter $string)
    (limiter-using (string-line-code $string) 1))

  (define (symbol-space-line-code-limiter $symbol)
    (limiter-using (symbol-line-code $symbol) 1))

  (define (bytevector-space-line-code?-limiter $bytevector)
    (case (bytevector-length $bytevector)
      ((1)
        (limiter-lets?
          ($bytevector-code (limiter-using (code "#bytevector") 1))
          ($byte-code (space-line-code?-limiter (bytevector-u8-ref $bytevector 0)))
          (limiter (space-separated-code $bytevector-code $byte-code))))
      (else (limiter #f))))

  (define (vector-space-line-code?-limiter $vector)
    (case (vector-length $vector)
      ((1)
        (limiter-lets?
          ($vector-code (limiter-using (code "#vector") 1))
          ($ref-code (space-line-code?-limiter (vector-ref $vector 0)))
          (limiter (space-separated-code $vector-code $ref-code))))
      (else (limiter #f))))

  (define (box-space-line-code?-limiter $box)
    (limiter-lets?
      ($box-code (limiter-using (code "#box") 1))
      ($ref-code (space-line-code?-limiter (unbox $box)))
      (limiter (space-separated-code $box-code $ref-code))))

  (define (pair-space-line-code?-limiter $pair)
    (limiter-lets?
      ($car-code (space-line-code?-limiter (car $pair)))
      (switch (cdr $pair)
        ((null? _)
          (limiter #f))
        ((pair? $cdr)
          (switch (cdr $cdr)
            ((null? _)
              (limiter-lets?
                ($cdr-code (space-line-code?-limiter (car $cdr)))
                (limiter (space-separated-code $car-code $cdr-code))))
            ((else _)
              (limiter #f))))
        ((else $cdr)
          (limiter-lets?
            ($cdr-code (space-line-code?-limiter $cdr))
            (limiter (dot-separated-code $car-code $cdr-code)))))))

  (define (other-space-line-code-limiter $other)
    (limiter-using (other-line-code $other) 1))

  (define (space-line-code?-limiter $datum)
    (switch $datum
      ((null? _) null-space-line-code-limiter)
      ((boolean? $boolean) (boolean-space-line-code-limiter $boolean))
      ((number? $number) (number-space-line-code-limiter $number))
      ((char? $char) (char-space-line-code-limiter $char))
      ((string? $string) (string-space-line-code-limiter $string))
      ((symbol? $symbol) (symbol-space-line-code-limiter $symbol))
      ((pair? $pair) (pair-space-line-code?-limiter $pair))
      ((box? $box) (box-space-line-code?-limiter $box))
      ((bytevector? $bytevector) (bytevector-space-line-code?-limiter $bytevector))
      ((vector? $vector) (vector-space-line-code?-limiter $vector))
      ((else $other) (other-space-line-code-limiter $other))))

  (define (limit-space-line-code? $limit $datum)
    (lets?
      ($limited (limiter-apply (space-line-code?-limiter $datum) $limit))
      (limited-ref $limited)))

  (define-rule-syntax (check-space-line-code size in out)
    (begin
      (check-code=? (limit-space-line-code? size in) out)
      (check (false? (limit-space-line-code? (- size 1) in)))))

  (define-rule-syntax (check-space-line-code-false? size in)
    (check (false? (limit-space-line-code? size in))))

  ; === colon-line-code-limiter ===

  (define null-colon-line-code-limiter
    null-space-line-code-limiter)

  (define (boolean-colon-line-code-limiter $boolean)
    (boolean-space-line-code-limiter $boolean))

  (define (number-colon-line-code-limiter $number)
    (number-space-line-code-limiter $number))

  (define (char-colon-line-code-limiter $char)
    (char-space-line-code-limiter $char))

  (define (string-colon-line-code-limiter $string)
    (string-space-line-code-limiter $string))

  (define (symbol-colon-line-code-limiter $symbol)
    (symbol-space-line-code-limiter $symbol))

  (define (bytevector-colon-line-code?-limiter $bytevector)
    (limiter-switch (bytevector-space-line-code?-limiter $bytevector)
      ((false? _)
        (limiter-lets
          ($bytevector-code (limiter-using (code "#bytevector") 1))
          ($ref-code?s (list->limiter (map space-line-code?-limiter (bytevector->u8-list $bytevector))))
          (limiter
            (and
              (for-all identity $ref-code?s)
              (code $bytevector-code
                (if (null? $ref-code?s) (code ":") (code ": "))
                (list->code (intercalate $ref-code?s comma-code)))))))
      ((else $code)
        (limiter $code))))

  (define (vector-colon-line-code?-limiter $vector)
    (limiter-switch (vector-space-line-code?-limiter $vector)
      ((false? _)
        (limiter-lets
          ($vector-code (limiter-using (code "#vector") 1))
          ($ref-code?s (list->limiter (map space-line-code?-limiter (vector->list $vector))))
          (limiter
            (and
              (for-all identity $ref-code?s)
              (code $vector-code
                (if (null? $ref-code?s) (code ":") (code ": "))
                (list->code (intercalate $ref-code?s comma-code)))))))
      ((else $code)
        (limiter $code))))

  (define (box-colon-line-code?-limiter $box)
    (box-space-line-code?-limiter $box))

  (define (pair-colon-line-code?-limiter $pair)
    (limiter-lets?
      ($car-code (colon-line-code?-limiter (car $pair)))
      (switch (cdr $pair)
        ((null? _)
          (limiter #f))
        ((pair? $cdr)
          (switch (cdr $cdr)
            ((null? _)
              (limiter-lets?
                ($cdr-code (colon-line-code?-limiter (car $cdr)))
                (limiter (space-separated-code $car-code $cdr-code))))
            ((else _)
              (limiter #f))))
        ((else $cdr)
          (limiter-lets?
            ($cdr-code (colon-line-code?-limiter $cdr))
            (limiter (dot-separated-code $car-code $cdr-code)))))))

  (define (other-colon-line-code-limiter $other)
    (limiter-using (other-line-code $other) 1))

  (define (colon-line-code?-limiter $datum)
    (switch $datum
      ((null? _) null-colon-line-code-limiter)
      ((boolean? $boolean) (boolean-colon-line-code-limiter $boolean))
      ((number? $number) (number-colon-line-code-limiter $number))
      ((char? $char) (char-colon-line-code-limiter $char))
      ((string? $string) (string-colon-line-code-limiter $string))
      ((symbol? $symbol) (symbol-colon-line-code-limiter $symbol))
      ((pair? $pair) (pair-colon-line-code?-limiter $pair))
      ((box? $box) (box-colon-line-code?-limiter $box))
      ((bytevector? $bytevector) (bytevector-colon-line-code?-limiter $bytevector))
      ((vector? $vector) (vector-colon-line-code?-limiter $vector))
      ((else $other) (other-colon-line-code-limiter $other))))

  (define (limit-colon-line-code? $limit $datum)
    (lets?
      ($limited (limiter-apply (colon-line-code?-limiter $datum) $limit))
      (limited-ref $limited)))

  (define-rule-syntax (check-colon-line-code size in out)
    (begin
      (check-code=? (limit-colon-line-code? size in) out)
      (check (false? (limit-colon-line-code? (- size 1) in)))))

  (define-rule-syntax (check-colon-line-code-false? size in)
    (check (false? (limit-colon-line-code? size in))))
)
