(library (leo sentence)
  (export
    quotify-for-display?
    skip-written?

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
    list->sentences)
  (import
    (scheme)
    (procedure-name)
    (syntaxes)
    (union)
    (switch)
    (pair)
    (lets)
    (char)
    (eof)
    (syntax)
    (procedure)
    (list)
    (list-syntax)
    (void))

  (define quotify-for-display? (make-thread-parameter #f))
  (define skip-written? (make-thread-parameter #f))

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

  (define (sentence-written $sentence)
    (cond
      ((skip-written?) $sentence)
      (else `("written" ,$sentence))))

  (define (null->sentence _)
    (sentence-written "null"))

  (define (void->sentence _)
    (sentence-written "void"))

  (define (boolean->sentence $boolean)
    (sentence-written (if $boolean "true" "false")))

  (define (number->sentence $number)
    (number->string $number))

  (define (char->sentence $char)
    (if (quotify-for-display?)
      (string $char)
      (sentence-written
        `("char"
          ,(->sentence (char->datum $char))))))

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
          `(
            ,(symbol->string $symbol)
            .
            ,(list->sentences (cdr $pair)))))
      ((else $other)
        `(
          ,(if (skip-written?) "list:" ":")
          . ,(list->sentences (cons $other (cdr $pair)))))))

  (define (box->sentence $box)
    (sentence-written
      `("box"
        ,(->sentence (unbox $box)))))

  (define (eof->sentence $eof)
    (sentence-written "eof"))

  (define (bytevector->sentence $bytevector)
    (sentence-written
      `("bytevector" . ,(list->sentences (bytevector->u8-list $bytevector)))))

  (define (vector->sentence $vector)
    (sentence-written
      `("vector" . ,(list->sentences (vector->list $vector)))))

  (define (ftype-pointer->sentence $ftype-pointer)
    (sentence-written
      (lets
        ($sentence
          `(
            ,(symbol->string (record-type-name (record-rtd $ftype-pointer)))
            ,(->sentence (ftype-pointer->sexpr $ftype-pointer))))
        (cond
          ((skip-written?) $sentence)
          (else `("ftype" ,$sentence))))))

  (define (record->sentence $record)
    (lets
      ($rtd (record-rtd $record))
      ($name (symbol->string (record-type-name $rtd)))
      ($sentence
        (if (zero? (vector-length (record-type-field-names $rtd)))
          $name
          `(
            ,(symbol->string (record-type-name $rtd))
            .
            ,(list->sentences
              (map-with
                ($index (iota (vector-length (record-type-field-names $rtd))))
                ((record-accessor $rtd $index) $record))))))
      (cond
        ((skip-written?) $sentence)
        (else (sentence-written `("record" ,$sentence))))))

  (define (procedure->sentence $procedure)
    (sentence-written
      (lets
        ($word "procedure")
        (switch (procedure-name? $procedure)
          ((symbol? $name) `(,$word ,(->sentence $name)))
          ((else _) $word)))))

  (define (syntax->sentence $syntax)
    ; TODO: Include annotation
    (sentence-written `("syntax" ,(->sentence (syntax->datum $syntax)))))

  (define (other->sentence $other)
    (format "#<~s>" $other))

  (define (->sentence $obj)
    (switch $obj
      ((null? $null) (null->sentence $null))
      ((void? $void) (void->sentence $void))
      ((boolean? $boolean) (boolean->sentence $boolean))
      ((number? $number) (number->sentence $number))
      ((string? $string) (string->sentence $string))
      ((symbol? $symbol) (symbol->sentence $symbol))
      ((pair? $pair) (pair->sentence $pair))
      ((char? $char) (char->sentence $char))
      ((box? $box) (box->sentence $box))
      ((eof? $eof) (eof->sentence $eof))
      ((bytevector? $bytevector) (bytevector->sentence $bytevector))
      ((vector? $vector) (vector->sentence $vector))
      ((procedure? $procedure) (procedure->sentence $procedure))
      ((syntax? $syntax) (syntax->sentence $syntax))
      ((ftype-pointer? $ftype-pointer) (ftype-pointer->sentence $ftype-pointer))
      ((record? $record) (record->sentence $record))
      ((else $other) (other->sentence $other))))

  (define (list->sentences $list)
    (map* ->sentence ->sentence $list))
)
