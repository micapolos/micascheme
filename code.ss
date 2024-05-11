(library (code)
  (export
    code
    code-string
    code-append
    empty-code
    char-code
    string-code
    number-code
    code-indent)
  (import (scheme) (lets) (procedure) (syntaxes) (switch))

  ; (typeof code) => (lambda ($line-start? $indent $port) $line-start?)

  (define-case-syntaxes
    ((code)
      #`(lambda ($line-start? $indent $port) $line-start?))
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

  (define empty-code
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
)
