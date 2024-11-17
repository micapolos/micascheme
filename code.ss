(library (code)
  (export
    code
    code-string
    code-append
    empty-code
    char-code
    string-code
    number-code
    code-indent

    separated-code
    space-separated-code
    comma-separated-code
    newline-separated-code
    emptyline-separated-code

    suffixed-code
    colon-ended-code
    newline-ended-code

    code-in-round-brackets
    code-in-square-brackets
    code-in-angle-brackets
    code-in-curly-brackets)
  (import (scheme) (lets) (list) (list-syntax) (procedure) (syntaxes) (switch))

  ; (typeof code) => (lambda ($line-start? $indent $port) $line-start?) or #f if empty

  (define-case-syntaxes
    ((code) #`(empty-code))
    ((code $item)
      (switch (datum $item)
        ((char? $char) #`(char-code $item))
        ((string? $string) #`(string-code $item))
        ((number? $number) #`(number-code $item))
        ((else _) #`$item)))
    ((code $item $item* ...)
      #`(code-append (code $item) (code $item* ...))))

  (define (code-string $code)
    (lets
      ($port (open-output-string))
      (run ($code #t 0 $port))
      (get-output-string $port)))

  (define (code-append . $code*)
    (lambda ($line-start? $indent $port)
      (fold-left
        (lambda ($line-start? $code)
          ($code $line-start? $indent $port))
        $line-start?
        $code*)))

  (define (empty-code)
    (lambda ($line-start? $indent $port)
      $line-start?))

  (define (char-code $char)
    (case $char
      ((#\newline)
        (lambda ($line-start? $indent $port)
          (write-char $char $port)
          #t))
      (else
        (lambda ($line-start? $indent $port)
          (if $line-start?
            (repeat $indent
              (write-char #\space $port)
              (write-char #\space $port)))
          (write-char $char $port)
          #f))))

  (define (string-code $string)
    (apply code-append
      (map char-code
        (string->list $string))))

  (define (number-code $number)
    (string-code (number->string $number)))

  (define (code-indent $code)
    (lambda ($line-start? $indent $port)
      ($code $line-start? (add1 $indent) $port)))

  (define-case-syntaxes
    ((separated-code $separator $code ...)
      #`(code
        #,@(intercalate
          (syntax->list #'($code ...))
          #'$separator)))
    ((suffixed-code $suffix $code ...)
      #`(code
        #,@(map-with
          ($code (syntax->list #'($code ...)))
          #`(code #,$code $suffix))))
    ((space-separated-code $code ...)
      #`(separated-code " " $code ...))
    ((comma-separated-code $code ...)
      #`(separated-code ", " $code ...))
    ((newline-separated-code $code ...)
      #`(separated-code "\n" $code ...))
    ((emptyline-separated-code $code ...)
      #`(separated-code "\n\n" $code ...))
    ((emptyline-separated-code $code ...)
      #`(separated-code "\n\n" $code ...))
    ((colon-ended-code $code ...)
      #`(suffixed-code ";" $code ...))
    ((newline-ended-code $code ...)
      #`(suffixed-code "\n" $code ...))
    ((code-in-round-brackets $code ...)
      #`(code "(" $code ... ")"))
    ((code-in-square-brackets $code ...)
      #`(code "[" $code ... "]"))
    ((code-in-angle-brackets $code ...)
      #`(code "<" $code ... ">"))
    ((code-in-curly-brackets $code ...)
      #`(code "{" $code ... "}")))
)
