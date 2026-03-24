(library (leo code)
  (export
    code-pretty?
    code-line-limit

    atom-code?
    check-atom-code?

    line-code

    check-space-line-code
    check-space-line-code-false?

    check-colon-line-code
    check-colon-line-code-false?

    block-code
    check-block-code)
  (import
    (micascheme)
    (leo datum)
    (leo sentence)
    (code))

  (define code-pretty? (make-thread-parameter #f))
  (define code-line-limit (make-thread-parameter 7))
  (define code-string-limit (make-thread-parameter 1.5))

  (define (primitive-string $string)
    (parameterize ((sentence-pretty? (code-pretty?)))
      (primitive-word $string)))

  (define (primitive-code $string)
    (string-code (primitive-string $string)))

  (define (primitive-code-limiter $string)
    (limiter-using (primitive-code $string) 1))

  (define arrow-code (code "->"))
  (define comma-separator-code (code ", "))
  (define pair-separator-code arrow-code)

  (define (pair-separated-code $car $cdr)
    (space-separated-code $car pair-separator-code $cdr))

  (define (prepare $datum)
    (switch $datum
      ((boolean? $datum) $datum)
      ((else $other)
        (or (->sentence? $other) $other))))

  ; === atom-code? ===

  (define null-atom-code (primitive-code "null"))

  (define (boolean-atom-code $boolean)
    (primitive-code (if $boolean "true" "false")))

  (define (number-atom-code $number)
    (string-code (number->string $number)))

  (define (string-atom-code $string)
    (string-code (format "~s" $string)))

  (define (symbol-atom-code $symbol)
    (string-code (format "~s" $symbol)))

  (define (pair-atom-code? $pair) #f)

  (define (sentence-atom-code? $sentence)
    (and
      (null? (sentence-args $sentence))
      (string-code (sentence-word $sentence))))

  (define (other-atom-code $other)
    (other-line-code $other))

  (define (atom-code? $datum)
    (switch (prepare $datum)
      ((null? _) null-atom-code)
      ((boolean? $boolean) (boolean-atom-code $boolean))
      ((number? $number) (number-atom-code $number))
      ((string? $string) (string-atom-code $string))
      ((symbol? $symbol) (symbol-atom-code $symbol))
      ((pair? $pair) (pair-atom-code? $pair))
      ((sentence? $sentence) (sentence-atom-code? $sentence))
      ((else $other) (other-atom-code $other))))

  (define-rule-syntax (check-atom-code? in out)
    (if out
      (check-code=? (atom-code? in) out)
      (check (false? (atom-code? in)))))

  ; === line-code? ===

  (define null-line-code (primitive-code "null"))

  (define (boolean-line-code $boolean)
    (boolean-atom-code $boolean))

  (define (number-line-code $number)
    (number-atom-code $number))

  (define (string-line-code $string)
    (string-atom-code $string))

  (define (symbol-line-code $symbol)
    (symbol-atom-code $symbol))

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
          (pair-separated-code $car-code (line-code $cdr))))))

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
              (code $car-code comma-separator-code (lines-code $cdr)))
            ((else $cdr)
              (pair-separated-code $car-code (line-code $cdr))))))))

  (define (sentence-line-code $sentence)
    (lets
      ($word-code (string-code (sentence-word $sentence)))
      (switch (sentence-args $sentence)
        ((null? _)
          (code-in-round-brackets $word-code))
        ((singleton-list? $args)
          (space-separated-code $word-code (line-code (car $args))))
        ((pair? $args)
          (space-separated-code $word-code
            (code-in-round-brackets (lines-code $args))))
        ((else $args)
          (pair-separated-code $word-code (line-code $args))))))

  (define (other-line-code $other)
    (string-code (format "~s" $other)))

  (define (line-code $datum)
    (switch (prepare $datum)
      ((null? _) null-line-code)
      ((boolean? $boolean) (boolean-line-code $boolean))
      ((number? $number) (number-line-code $number))
      ((string? $string) (string-line-code $string))
      ((symbol? $symbol) (symbol-line-code $symbol))
      ((pair? $pair) (pair-line-code $pair))
      ((sentence? $sentence) (sentence-line-code $sentence))
      ((else $other) (other-line-code $other))))

  ; === space-line-code-limiter ===

  (define null-space-line-code-limiter
    (limiter-using null-line-code 1))

  (define (boolean-space-line-code-limiter $boolean)
    (limiter-using (boolean-line-code $boolean) 1))

  (define (number-space-line-code-limiter $number)
    (limiter-using (number-line-code $number) 1))

  (define (string-space-line-code-limiter $string)
    (limiter-using (string-line-code $string) (code-string-limit)))

  (define (symbol-space-line-code-limiter $symbol)
    (limiter-using (symbol-line-code $symbol) 1))

  (define (pair-space-line-code?-limiter $pair)
    (limiter-lets?
      ($car-code (limiter-using (atom-code? (car $pair)) 1))
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
            (limiter (pair-separated-code $car-code $cdr-code)))))))

  (define (sentence-space-line-code?-limiter $sentence)
    (limiter-lets?
      ($word-code (limiter-using (string-code (sentence-word $sentence)) 1))
      (switch (sentence-args $sentence)
        ((null? _)
          (limiter #f))
        ((pair? $cdr)
          (switch (cdr $cdr)
            ((null? _)
              (limiter-lets?
                ($cdr-code (space-line-code?-limiter (car $cdr)))
                (limiter (space-separated-code $word-code $cdr-code))))
            ((else _)
              (limiter #f))))
        ((else $cdr)
          (limiter-lets?
            ($cdr-code (space-line-code?-limiter $cdr))
            (limiter (pair-separated-code $word-code $cdr-code)))))))

  (define (other-space-line-code-limiter $other)
    (limiter-using (other-line-code $other) 1))

  (define (space-line-code?-limiter $datum)
    (switch (prepare $datum)
      ((null? _) null-space-line-code-limiter)
      ((boolean? $boolean) (boolean-space-line-code-limiter $boolean))
      ((number? $number) (number-space-line-code-limiter $number))
      ((string? $string) (string-space-line-code-limiter $string))
      ((symbol? $symbol) (symbol-space-line-code-limiter $symbol))
      ((pair? $pair) (pair-space-line-code?-limiter $pair))
      ((sentence? $sentence) (sentence-space-line-code?-limiter $sentence))
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

  (define (string-colon-line-code-limiter $string)
    (string-space-line-code-limiter $string))

  (define (symbol-colon-line-code-limiter $symbol)
    (symbol-space-line-code-limiter $symbol))

  (define (pair-colon-line-code?-limiter $pair)
    (switch (limiter-unlimited-ref (pair-space-line-code?-limiter $pair))
      ((false? _)
        (switch (car $pair)
          ((symbol? $symbol)
            (limiter-lets
              ($symbol-code (limiter-using (symbol-atom-code $symbol) 1))
              (switch (cdr $pair)
                ((singleton-list? $cdr)
                  (limiter-lets?
                    ($cdr-code (colon-line-code?-limiter (car $cdr)))
                    (limiter (space-separated-code $symbol-code $cdr-code))))
                ((else $cdr)
                  (limiter-lets?
                    ($cdr-code (list-colon-line-code?-limiter $cdr))
                    (limiter (code $symbol-code $cdr-code)))))))
            ((else _)
              (list-colon-line-code?-limiter $pair))))
      ((else _)
        (pair-space-line-code?-limiter $pair))))

  (define (list-colon-line-code?-limiter $list)
    (switch $list
      ((null? _) (limiter (code ":")))
      ((else $list)
        (limiter-lets
          ($code?s
            (list->limiter
              (map*
                space-line-code?-limiter
                (lambda ($item)
                  (limiter-lets
                    ($line-code (space-line-code?-limiter $item))
                    (limiter (space-separated-code pair-separator-code $line-code))))
                $list)))
          (limiter
            (and
              (for-all identity $code?s)
              (code ": " (list->code (intercalate $code?s comma-separator-code)))))))))

  (define (sentence-colon-line-code?-limiter $sentence)
    (switch (limiter-unlimited-ref (sentence-space-line-code?-limiter $sentence))
      ((false? _)
        (limiter-lets
          ($symbol-code (limiter-using (string-code (sentence-word $sentence)) 1))
          (switch (sentence-args $sentence)
            ((singleton-list? $cdr)
              (limiter-lets?
                ($cdr-code (colon-line-code?-limiter (car $cdr)))
                (limiter (space-separated-code $symbol-code $cdr-code))))
            ((else $cdr)
              (limiter-lets?
                ($cdr-code (list-colon-line-code?-limiter $cdr))
                (limiter (code $symbol-code $cdr-code)))))))
      ((else _)
        (sentence-space-line-code?-limiter $sentence))))

  (define (other-colon-line-code-limiter $other)
    (limiter-using (other-line-code $other) 1))

  (define (colon-line-code?-limiter $datum)
    (switch (prepare $datum)
      ((null? _) null-colon-line-code-limiter)
      ((boolean? $boolean) (boolean-colon-line-code-limiter $boolean))
      ((number? $number) (number-colon-line-code-limiter $number))
      ((string? $string) (string-colon-line-code-limiter $string))
      ((symbol? $symbol) (symbol-colon-line-code-limiter $symbol))
      ((pair? $pair) (pair-colon-line-code?-limiter $pair))
      ((sentence? $sentence) (sentence-colon-line-code?-limiter $sentence))
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

  ; === block-code ===

  (define (limiter-line-code? $line-code?-limiter)
    (limiter-ref? $line-code?-limiter (code-line-limit)))

  (define null-block-code
    (newline-ended-code null-line-code))

  (define (boolean-block-code $boolean)
    (newline-ended-code (boolean-line-code $boolean)))

  (define (number-block-code $number)
    (newline-ended-code (number-line-code $number)))

  (define (string-block-code $string)
    (newline-ended-code (string-line-code $string)))

  (define (symbol-block-code $symbol)
    (newline-ended-code (symbol-line-code $symbol)))

  (define (pair-block-code $pair)
    (switch (limiter-line-code? (pair-colon-line-code?-limiter $pair))
      ((false? _)
        (switch (car $pair)
          ((symbol? $symbol)
            (lets
              ($symbol-code (symbol-atom-code $symbol))
              (switch (cdr $pair)
                ((singleton-list? $cdr)
                  (space-separated-code $symbol-code
                    (block-code (car $cdr))))
                ((else $cdr)
                  (code $symbol-code #\newline
                    (code-indent (list-block-code (cdr $pair))))))))
          ((else _)
            (code ":" #\newline
              (code-indent (list-block-code $pair))))))
      ((else $code)
        (newline-ended-code $code))))

  (define (list-block-code $list)
    (list->code
      (map*
        block-code
        (lambda ($item) (space-separated-code pair-separator-code (block-code $item)))
        $list)))


  (define (sentence-block-code $sentence)
    (switch (limiter-line-code? (sentence-colon-line-code?-limiter $sentence))
      ((false? _)
        (lets
          ($symbol-code (string-code (sentence-word $sentence)))
          (switch (sentence-args $sentence)
            ((singleton-list? $cdr)
              (space-separated-code $symbol-code
                (block-code (car $cdr))))
            ((else $cdr)
              (code $symbol-code #\newline
                (code-indent (list-block-code $cdr)))))))
      ((else $code)
        (newline-ended-code $code))))

  (define (other-block-code $other)
    (newline-ended-code (string-code (format "~s" $other))))

  (define (block-code $datum)
    (switch (prepare $datum)
      ((null? _) null-block-code)
      ((boolean? $boolean) (boolean-block-code $boolean))
      ((number? $number) (number-block-code $number))
      ((string? $string) (string-block-code $string))
      ((symbol? $symbol) (symbol-block-code $symbol))
      ((pair? $pair) (pair-block-code $pair))
      ((sentence? $sentence) (sentence-block-code $sentence))
      ((else $other) (other-block-code $other))))

  (define-rule-syntax (check-block-code in out ...)
    (check-code=? (block-code in) (lines-string out ...)))
)
