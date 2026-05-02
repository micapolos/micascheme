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
    (system)
    (procedure)
    (code))

  (define code-pretty? (make-thread-parameter #f))
  (define code-line-limit (make-thread-parameter 7))
  (define code-string-limit (make-thread-parameter 1))

  (define comma-code (code ","))
  (define comma-separator-code (code ", "))

  (define (terminator-code $code) (code $code "."))

  (define (->code-sentence $datum)
    (sentence-quotify (->sentence $datum)))

  ; === atom-code? ===

  (define (string-atom-code $string)
    (string-code $string))

  (define (phrase-atom-code? $pair) #f)

  (define (sentence-atom-code? $sentence)
    (sentence-switch $sentence
      ((string? $string) (string-code $string))
      ((phrase? $phrase) (phrase-atom-code? $phrase))))

  (define (atom-code? $datum)
    (sentence-atom-code? (->code-sentence $datum)))

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
          ((null/pair? _) (code-in-round-brackets $lines-code))
          ((else _) $lines-code)))
      (switch (phrase-string? $phrase)
        ((string? $string)
          (space-separated-code (string-code $string) $body-code))
        ((else _)
          (code-in-round-brackets $lines-code)))))

  (define (lines-code $lines)
    (list->code
      (intercalate
        (map*
          sentence-line-code
          sentence-tail-line-code-tail
          $lines)
        comma-separator-code)))

  (define (sentence-line-code $sentence)
    (sentence-switch $sentence
      ((string? $string) (string-line-code $string))
      ((phrase? $phrase) (phrase-line-code $phrase))))

  (define (sentence-tail-line-code-tail $sentence-tail)
    (switch $sentence-tail
      ((null? $null) $null)
      ((else $sentence)
        (list (terminator-code (sentence-line-code $sentence))))))

  (define (line-code $datum)
    (sentence-line-code (->code-sentence $datum)))

  ; === space-line-code-limiter ===

  (define (string-space-line-code-limiter $string)
    (limiter-using (string-line-code $string) (code-string-limit)))

  (define (phrase-space-line-code?-limiter $phrase)
    (switch (phrase-string? $phrase)
      ((string? $word)
        (limiter-lets?
          ($string-code (limiter-using (string-atom-code $word) 1))
          (switch (phrase-body $phrase)
            ((singleton-list? $list)
              (limiter-lets?
                ($cdr-code (sentence-space-line-code?-limiter (car $list)))
                (limiter (space-separated-code $string-code $cdr-code))))
            ((null/pair? _)
              (limiter #f))
            ((else $terminator)
              (limiter-lets?
                ($code (sentence-space-line-code?-limiter $terminator))
                (limiter (space-separated-code $string-code (terminator-code $code))))))))
      ((else _)
        (limiter #f))))

  (define (sentence-space-line-code?-limiter $sentence)
    (sentence-switch $sentence
      ((string? $string) (string-space-line-code-limiter $string))
      ((phrase? $phrase) (phrase-space-line-code?-limiter $phrase))))

  (define (space-line-code?-limiter $datum)
    (sentence-space-line-code?-limiter (->code-sentence $datum)))

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
          ($string-code (limiter-using (string-atom-code (or (phrase-string? $phrase) ":")) 1))
          (switch (phrase-body $phrase)
            ((singleton-list? $list)
              (limiter-lets?
                ($code (sentence-colon-line-code?-limiter (car $list)))
                (limiter (space-separated-code $string-code $code))))
            ((null/pair? $list)
              (limiter-lets?
                ($code (list-colon-line-code?-limiter $list))
                (limiter (code $string-code $code))))
            ((else $terminator)
              (limiter-lets?
                ($code (sentence-colon-line-code?-limiter $terminator))
                (limiter (code $string-code (terminator-code $code))))))))
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
                sentence-space-line-code?-limiter
                sentence-tail-colon-line-code?limiter-tail
                $list)))
          (limiter
            (and
              (for-all identity $code?s)
              (code ": " (list->code (intercalate $code?s comma-separator-code)))))))))

  (define (sentence-colon-line-code?-limiter $sentence)
    (sentence-switch $sentence
      ((string? $string) (string-colon-line-code-limiter $string))
      ((phrase? $phrase) (phrase-colon-line-code?-limiter $phrase))))

  (define (sentence-tail-colon-line-code?limiter-tail $sentence-tail)
    (switch $sentence-tail
      ((null? $null) $null)
      ((else $sentence)
        (list
          (limiter-lets?
            ($code (sentence-colon-line-code?-limiter $sentence))
            (limiter (terminator-code $code)))))))

  (define (colon-line-code?-limiter $datum)
    (sentence-colon-line-code?-limiter (->code-sentence $datum)))

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
          ($string-code (string-atom-code (or (phrase-string? $phrase) ":")))
          (switch (phrase-body $phrase)
            ((singleton-list? $cdr)
              (space-separated-code $string-code
                (sentence-block-code (car $cdr))))
            ((null/pair? $cdr)
              (code $string-code #\newline
                (code-indent (sentence-list-block-code (phrase-body $phrase)))))
            ((else $sentence)
              (space-separated-code $string-code
                (terminator-code (sentence-block-code $sentence)))))))
      ((else $code)
        (newline-ended-code $code))))

  (define (sentence-list-block-code $sentence-list)
    (list->code
      (map*
        sentence-block-code
        sentence-tail-block-code-tail
        $sentence-list)))

  (define (sentence-block-code $sentence)
    (sentence-switch $sentence
      ((string? $string) (string-block-code $string))
      ((phrase? $phrase) (phrase-block-code $phrase))))

  (define (sentence-tail-block-code-tail $sentence-tail)
    (switch $sentence-tail
      ((null? $null) $null)
      ((string? $string)
        (list (newline-ended-code (terminator-code (string-line-code $string)))))
      ((else $sentence)
        (list
          (newline-ended-code
            (terminator-code
              (sentence-block-code $sentence)))))))

  (define (block-code $datum)
    (sentence-block-code (->code-sentence $datum)))

  (define-rule-syntax (check-block-code in out ...)
    (check-code=? (block-code in) (lines-string out ...)))
)
