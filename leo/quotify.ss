(library (leo quotify)
  (export
    quotify-for-display?

    phrase-cons phrase? phrase-string phrase-body
    sentence? sentence-switch
    phrase sentence

    quote-string
    quote-phrase?
    quote-sentence?

    unquote-string
    unquote-phrase?
    unquote-sentence?

    sentence-quotify

    ->sentence
    list->sentences
    normalize-list)
  (import
    (scheme)
    (procedure-name)
    (syntaxes)
    (union)
    (switch)
    (pair)
    (lets)
    (char)
    (syntax)
    (procedure)
    (list)
    (list-syntax))

  (define quotify-for-display? (make-thread-parameter #f))

  (define (phrase-cons word body) (cons word body))
  (define phrase? pair?)
  (define phrase-string car)
  (define phrase-body cdr)

  (define-rules-syntaxes
    ((phrase (word x ...)) (phrase-cons word (list 'x ...)))
    ((phrase (word x ... . tail)) (phrase-cons word (list* 'x ... 'tail)))
    ((sentence (word . tail)) (phrase (word . tail)))
    ((sentence word) word))

  (union (sentence string phrase))

  ; === quote / unquote

  (define (quote-string $quote $string)
    (string-append $quote $string))

  (define (quote-phrase? $quote $phrase)
    (phrase-cons
      (quote-string $quote (phrase-string $phrase))
      (phrase-body $phrase)))

  (define (quote-sentence? $quote $sentence)
    (sentence-switch $sentence
      ((string? $string) (quote-string $quote $string))
      ((phrase? $phrase) (quote-phrase? $quote $phrase))))

  (define (unquote-string $unquote $string)
    (string-append $string $unquote))

  (define (unquote-phrase? $unquote $phrase)
    (phrase-cons
      (unquote-string $unquote (phrase-string $phrase))
      (phrase-body $phrase)))

  (define (unquote-sentence? $unquote $sentence)
    (sentence-switch $sentence
      ((string? $word) (unquote-string $unquote $word))
      ((phrase? $phrase) (unquote-phrase? $unquote $phrase))))

  ; === quotify

  (define (begin-string? $word)
    (case $word
      (("quote") "'")
      (("quasiquote") "`")
      (else #f)))

  (define (end-string? $word)
    (case $word
      (("unquote") "`")
      (("unquote-splicing") "`...")
      (else #f)))

  (define (sentence-quotify $sentence)
    (sentence-switch $sentence
      ((string? $string) $string)
      ((phrase? $phrase)
        (or
          (switch? (phrase-body $phrase)
            ((singleton-list? $body)
              (lets
                ($body-sentence (->sentence (car $body)))
                (switch (begin-string? (phrase-string $phrase))
                  ((string? $quote)
                    (quote-sentence? $quote $body-sentence))
                  ((else _)
                    (switch? $body-sentence
                      ((phrase? $body-phrase)
                        (lets
                          ($body-body (phrase-body $body-phrase))
                          (switch? (end-string? (phrase-string $body-phrase))
                            ((string? $unquote)
                              (unquote-sentence? $unquote
                                (phrase-cons
                                  (phrase-string $phrase)
                                  (phrase-body $body-phrase)))))))))))))
            $phrase))))

  ; === ->sentence

  (define (null->sentence _) "null")

  (define (boolean->sentence $boolean)
    (if $boolean "true" "false"))

  (define (number->sentence $number)
    (number->string $number))

  (define (char->sentence $char)
    (if (quotify-for-display?)
      (string $char)
      (phrase-cons "char" (list (char->datum $char)))))

  (define (string->sentence $string)
    (if (quotify-for-display?)
      $string
      (format "~s" $string)))

  (define (symbol->sentence $symbol)
    (format "~s" $symbol))

  (define (pair->sentence $pair)
    (switch (car $pair)
      ((symbol? $symbol)
        (sentence-quotify
          (phrase-cons
            (symbol->string $symbol)
            (normalize-list (cdr $pair)))))
      ((else $other)
        (phrase-cons "list"
          (normalize-list (cons $other (cdr $pair)))))))

  (define (box->sentence $box)
    (phrase-cons "box"
      (list (unbox $box))))

  (define (bytevector->sentence $bytevector)
    (phrase-cons "bytevector"
      (bytevector->u8-list $bytevector)))

  (define (vector->sentence $vector)
    (phrase-cons "vector"
      (vector->list $vector)))

  (define (record->sentence $record)
    (lets
      ($rtd (record-rtd $record))
      (phrase-cons
        (symbol->string (record-type-name $rtd))
        (map-with
          ($index (iota (vector-length (record-type-field-names $rtd))))
          ((record-accessor $rtd $index) $record)))))

  (define (procedure->sentence $procedure)
    (lets
      ($word "procedure")
      (switch (procedure-name? $procedure)
        ((symbol? $name)
          (phrase-cons $word `(,$name)))
        ((else _) $word))))

  (define (syntax->sentence $syntax)
    ; TODO: Include annotation
    (phrase-cons "syntax"
      (list (syntax->datum $syntax))))

  (define (other->sentence $other)
    (format "#<~s>" $other))

  (define (->sentence $obj)
    (switch $obj
      ((null? $null) (null->sentence $null))
      ((boolean? $boolean) (boolean->sentence $boolean))
      ((number? $number) (number->sentence $number))
      ((string? $string) (string->sentence $string))
      ((symbol? $symbol) (symbol->sentence $symbol))
      ((pair? $pair) (pair->sentence $pair))
      ((char? $char) (char->sentence $char))
      ((box? $box) (box->sentence $box))
      ((bytevector? $bytevector) (bytevector->sentence $bytevector))
      ((vector? $vector) (vector->sentence $vector))
      ((procedure? $procedure) (procedure->sentence $procedure))
      ((syntax? $syntax) (syntax->sentence $syntax))
      ((record? $record) (record->sentence $record))
      ((else $other) (other->sentence $other))))

  (define (list->sentences $list)
    (map*
      ->sentence
      (lambda ($item) (list (phrase-cons "and" (list $item))))
      $list))

  (define (normalize-list $list)
    (map*
      identity
      (lambda ($item) `((and ,$item)))
      $list))
)
