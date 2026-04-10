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
    (scheme)
    (check)
    (leo datum)
    (leo sentence)
    (syntax)
    (lets)
    (switch)
    (pair)
    (list)
    (limiter)
    (string)
    (limited)
    (boolean)
    (procedure)
    (code))

  (define code-pretty? (make-thread-parameter #f))
  (define code-line-limit (make-thread-parameter 7))
  (define code-string-limit (make-thread-parameter 1))

  (define comma-code (code ","))
  (define comma-separator-code (code ", "))

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
      ($body (phrase-body $phrase))
      ($lines-code (lines-code $body))
      ($body-code
        (switch $body
          ((singleton-list? _) $lines-code)
          ((else _) (code-in-round-brackets $lines-code))))
      (space-separated-code
        (string-code (phrase-string $phrase))
        $body-code)))

  (define (lines-code $lines)
    (list->code
      (intercalate
        (map line-code (normalize-list $lines))
        comma-separator-code)))

  (define (line-code $datum)
    (sentence-switch (->sentence $datum)
      ((string? $string) (string-line-code $string))
      ((phrase? $phrase) (phrase-line-code $phrase))))

  ; === space-line-code-limiter ===

  (define (string-space-line-code-limiter $string)
    (limiter-using (string-line-code $string) (code-string-limit)))

  (define (phrase-space-line-code?-limiter $phrase)
    (limiter-lets?
      ($string-code (limiter-using (string-atom-code (phrase-string $phrase)) 1))
      (switch (phrase-body $phrase)
        ((null? _)
          (limiter #f))
        ((singleton-list? $list)
          (limiter-lets?
            ($cdr-code (space-line-code?-limiter (car $list)))
            (limiter (space-separated-code $string-code $cdr-code))))
        ((pair? $cdr)
          (limiter #f)))))

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
        (limiter-lets
          ($string-code (limiter-using (string-atom-code (phrase-string $phrase)) 1))
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
        (phrase-space-line-code?-limiter $phrase))))

  (define (list-colon-line-code?-limiter $list)
    (switch $list
      ((null? _) (limiter (code ":")))
      ((else $list)
        (limiter-lets
          ($code?s (list->limiter (map space-line-code?-limiter $list)))
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
        (lets
          ($string-code (string-atom-code (phrase-string $phrase)))
          (switch (phrase-body $phrase)
            ((singleton-list? $cdr)
              (space-separated-code $string-code
                (block-code (car $cdr))))
            ((else $cdr)
              (code $string-code #\newline
                (code-indent (list-block-code (phrase-body $phrase))))))))
      ((else $code)
        (newline-ended-code $code))))

  (define (list-block-code $list)
    (list->code (map block-code $list)))

  (define (block-code $datum)
    (sentence-switch (->sentence $datum)
      ((string? $string) (string-block-code $string))
      ((phrase? $phrase) (phrase-block-code $phrase))))

  (define-rule-syntax (check-block-code in out ...)
    (check-code=? (block-code in) (lines-string out ...)))
)
