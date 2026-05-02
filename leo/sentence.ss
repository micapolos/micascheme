(library (leo sentence)
  (export
    quotify-for-display?
    pretty-write?

    phrase-cons phrase? phrase-string? phrase-body
    sentence? sentence-switch
    phrase sentence

    quote-string
    quote-string?
    quote-phrase?
    quote-sentence?

    unquote-string
    unquote-string?
    unquote-phrase?
    unquote-sentence?

    sentence-quotify
    depth/sentence-quotify

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
    (system)
    (void))

  (define quotify-for-display? (make-thread-parameter #f))
  (define pretty-write? (make-thread-parameter #f))

  (define (phrase-cons word body) (cons word body))
  (define phrase? pair?)
  (define phrase-string? car)
  (define phrase-body cdr)

  (define (phrase-simple-body? $phrase)
    (switch? (phrase-body $phrase)
      ((singleton-list? $list) (car $list))))

  (define (sentence-simple-body? $sentence)
    (switch? $sentence
      ((phrase? $phrase)
        (phrase-simple-body? $phrase))))

  (define (string?/phrase-simple-body? $string? $phrase)
    (and
      (equal? $string? (phrase-string? $phrase))
      (phrase-simple-body? $phrase)))

  (define (string?/sentence-simple-body? $string? $sentence)
    (switch? $sentence
      ((phrase? $phrase)
        (string?/phrase-simple-body? $string? $phrase))))

  (define-rules-syntaxes
    ((phrase (word x ...)) (phrase-cons word (list 'x ...)))
    ((phrase (word x ... . tail)) (phrase-cons word (list* 'x ... 'tail)))
    ((sentence (word . tail)) (phrase (word . tail)))
    ((sentence word) word))

  (union (sentence string phrase))

  ; === quote / unquote

  (define (quote-string $quote $string)
    (string-append $quote $string))

  (define (quote-string? $quote-word $quote $string?)
    (if $string?
      (quote-string $quote $string?)
      $quote-word))

  (define (quote-phrase? $quote-word $quote $phrase)
    (phrase-cons
      (quote-string? $quote-word $quote (phrase-string? $phrase))
      (phrase-body $phrase)))

  (define (quote-sentence? $quote-word $quote $sentence)
    (sentence-switch $sentence
      ((string? $string) (quote-string $quote $string))
      ((phrase? $phrase) (quote-phrase? $quote-word $quote $phrase))))

  (define (unquote-string $unquote $string)
    (string-append $string $unquote))

  (define (unquote-string? $unquote-word $unquote $string?)
    (if $string?
      (unquote-string $unquote $string?)
      $unquote-word))

  (define (unquote-phrase? $unquote-word $unquote $phrase)
    (phrase-cons
      (unquote-string? $unquote-word $unquote (phrase-string? $phrase))
      (phrase-body $phrase)))

  (define (unquote-sentence? $unquote-word $unquote $sentence)
    (sentence-switch $sentence
      ((string? $word) (unquote-string $unquote $word))
      ((phrase? $phrase) (unquote-phrase? $unquote-word $unquote $phrase))))

  ; === quotify

  (define (begin-string? $word?)
    (and $word?
      (cond
        ((string=? $word? "quasiquote") "'")
        (else #f))))

  (define (end-string? $word?)
    (and $word?
      (cond
        ((string=? $word? "unquote") "'")
        ((string=? $word? "unquote-splicing") "'.")
        (else #f))))

  (define (depth/string-quotify $depth $string)
    (cond
      ((zero? $depth)
        $string)
      (else
        (depth/string-quotify
          (- $depth 1)
          (string-append $string "'")))))

  (define (depth/phrase-quotify $depth $phrase)
    (lets
      ($word? (phrase-string? $phrase))
      ($body (phrase-body $phrase))
      (cond
        (
          (and
            (equal? $word? "quasiquote")
            (singleton-list? $body))
          (quote-sentence? $word? "'"
            (depth/sentence-quotify (+ $depth 1) (car $body))))
        (
          (and
            $word?
            (not (zero? $depth))
            (for-all* (partial string?/sentence-simple-body? "unquote") $body))
          (phrase-cons
            (unquote-string "'" $word?)
            (map*
              (partial depth/sentence-quotify (- $depth 1))
              (partial depth/sentence-quotify (- $depth 1))
              (map* sentence-simple-body? sentence-simple-body? $body))))
        (
          (and
            $word?
            (not (zero? $depth))
            (for-all* (partial string?/sentence-simple-body? "unquote-splicing") $body))
          (phrase-cons
            (unquote-string "'..." $word?)
            (map*
              (partial depth/sentence-quotify (- $depth 1))
              (partial depth/sentence-quotify (- $depth 1))
              (map* sentence-simple-body? sentence-simple-body? $body))))
        (else
          (phrase-cons $word?
            (map*
              (partial depth/sentence-quotify $depth)
              (partial depth/sentence-quotify $depth)
              $body))))))

  (define (depth/sentence-quotify $depth $sentence)
    (sentence-switch $sentence
      ((string? $string) (depth/string-quotify $depth $string))
      ((phrase? $phrase) (depth/phrase-quotify $depth $phrase))))

  (define (sentence-quotify $sentence)
    (depth/sentence-quotify 0 $sentence))

  ; === ->sentence

  (define (sentence-written $sentence)
    (cond
      ((pretty-write?) $sentence)
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
        `(
          ,(symbol->string $symbol)
          .
          ,(list->sentences (cdr $pair))))
      ((else $other)
        `(
          ,(if (pretty-write?) "list" #f)
          .
          ,(list->sentences (cons $other (cdr $pair)))))))

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
          ((pretty-write?) $sentence)
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
        ((pretty-write?) $sentence)
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
