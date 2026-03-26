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
  (define code-string-limit (make-thread-parameter 1))

  (define (primitive-code $string)
    (string-code (primitive-string $string)))

  (define (primitive-code-limiter $string)
    (limiter-using (primitive-code $string) 1))

  (define arrow-code (code "->"))
  (define comma-code (code ","))
  (define comma-separator-code (code ", "))
  (define pair-separator-code arrow-code)

  (define (pair-separated-code $car $cdr)
    (space-separated-code $car pair-separator-code $cdr))

  ; === atom-code? ===

  (define (string-atom-code $string)
    (string-code $string))

  (define (phrase-atom-code? $pair) #f)

  (define (atom-code? $datum)
    (sentence-switch (->sentence $datum)
      ((string? $string) (string-code $string))
      ((phrase? $phrase) (phrase-atom-code? $phrase))))

  (define-rule-syntax (check-atom-code? in out)
    (if out
      (check-code=? (atom-code? in) out)
      (check (false? (atom-code? in)))))

  ; === line-code ===

  (define (string-line-code $string)
    (string-atom-code $string))

  (define (phrase-line-code $phrase)
    (lets
      ($lines-code (lines-code (phrase-body $phrase)))
      ($body-code
        (switch (phrase-body $phrase)
          ((singleton-list? _) $lines-code)
          ((null/pair? _) (code-in-round-brackets $lines-code))
          ((else _) (space-separated-code pair-separator-code $lines-code))))
      (switch (phrase-string? $phrase)
        ((string? $string)
          (space-separated-code
            (string-code $string)
            $body-code))
        ((else _) (code-in-round-brackets $body-code)))))

  (define (lines-code $lines)
    (switch $lines
      ((null? _) (code))
      ((pair? $pair)
        (lets
          ($car-code (line-code (car $pair)))
          ($cdr-code
            (switch
              (map*
                (lambda ($item)
                  (code comma-code #\space (line-code $item)))
                (lambda ($item)
                  (code #\space pair-separator-code #\space (line-code $item)))
                (cdr $pair))
              ((null? $codes) (list->code $codes))
              ((pair? $codes) (list->code $codes))
              ((else $code) $code)))
          (code $car-code $cdr-code)))
      ((else $other)
        (line-code $other))))

  (define (line-code $datum)
    (sentence-switch (->sentence $datum)
      ((string? $string) (string-line-code $string))
      ((phrase? $phrase) (phrase-line-code $phrase))))

  ; === space-line-code-limiter ===

  (define (string-space-line-code-limiter $string)
    (limiter-using (string-line-code $string) (code-string-limit)))

  (define (phrase-space-line-code?-limiter $phrase)
    (switch (phrase-string? $phrase)
      ((string? $string)
        (limiter-lets?
          ($string-code (limiter-using (string-atom-code $string) 1))
          (switch (phrase-body $phrase)
            ((null? _)
              (limiter #f))
            ((singleton-list? $list)
              (limiter-lets?
                ($cdr-code (space-line-code?-limiter (car $list)))
                (limiter (space-separated-code $string-code $cdr-code))))
            ((pair? $cdr)
              (limiter #f))
            ((else $other)
              (limiter-lets?
                ($other-code (space-line-code?-limiter $other))
                (limiter (pair-separated-code $string-code $other-code)))))))
      ((else _)
        (limiter #f))))

  (define (space-line-code?-limiter $datum)
    (sentence-switch (->sentence $datum)
      ((string? $string) (string-space-line-code-limiter $string))
      ((phrase? $phrase) (phrase-space-line-code?-limiter $phrase))))

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

  (define (string-colon-line-code-limiter $string)
    (string-space-line-code-limiter $string))

  (define (phrase-colon-line-code?-limiter $phrase)
    (switch (limiter-unlimited-ref (phrase-space-line-code?-limiter $phrase))
      ((false? _)
        (switch (phrase-string? $phrase)
          ((string? $string)
            (limiter-lets
              ($string-code (limiter-using (string-atom-code $string) 1))
              (switch (phrase-body $phrase)
                ((singleton-list? $cdr)
                  (limiter-lets?
                    ($cdr-code (colon-line-code?-limiter (car $cdr)))
                    (limiter (space-separated-code $string-code $cdr-code))))
                ((else $cdr)
                  (limiter-lets?
                    ($cdr-code (list-colon-line-code?-limiter $cdr))
                    (limiter (code $string-code $cdr-code)))))))
            ((else _)
              (list-colon-line-code?-limiter (phrase-body $phrase)))))
      ((else _)
        (phrase-space-line-code?-limiter $phrase))))

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

  (define (colon-line-code?-limiter $datum)
    (sentence-switch (->sentence $datum)
      ((string? $string) (string-colon-line-code-limiter $string))
      ((phrase? $phrase) (phrase-colon-line-code?-limiter $phrase))))

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

  (define (string-block-code $string)
    (newline-ended-code (string-line-code $string)))

  (define (phrase-block-code $phrase)
    (switch (limiter-line-code? (phrase-colon-line-code?-limiter $phrase))
      ((false? _)
        (switch (phrase-string? $phrase)
          ((string? $string)
            (lets
              ($string-code (string-atom-code $string))
              (switch (phrase-body $phrase)
                ((singleton-list? $cdr)
                  (space-separated-code $string-code
                    (block-code (car $cdr))))
                ((else $cdr)
                  (code $string-code #\newline
                    (code-indent (list-block-code (phrase-body $phrase))))))))
          ((else _)
            (code ":" #\newline
              (code-indent (list-block-code (phrase-body $phrase)))))))
      ((else $code)
        (newline-ended-code $code))))

  (define (list-block-code $list)
    (list->code
      (map*
        block-code
        (lambda ($item) (space-separated-code pair-separator-code (block-code $item)))
        $list)))

  (define (block-code $datum)
    (sentence-switch (->sentence $datum)
      ((string? $string) (string-block-code $string))
      ((phrase? $phrase) (phrase-block-code $phrase))))

  (define-rule-syntax (check-block-code in out ...)
    (check-code=? (block-code in) (lines-string out ...)))
)
