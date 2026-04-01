(library (char)
  (export
    char->ascii
    char-newline?
    char-space?
    char-comma?
    char-dot?
    char-colon?
    char-left-parenthesis?
    char
    char->datum)
  (import
    (scheme)
    (integer)
    (syntax)
    (only (code) code))
  (export (import (only (code) code)))

  (define (char->ascii $char)
    (bitwise-and #xff (char->integer $char)))

  (define (char-newline? $char) (char=? $char #\newline))
  (define (char-space? $char) (char=? $char #\space))
  (define (char-comma? $char) (char=? $char #\,))
  (define (char-dot? $char) (char=? $char #\.))
  (define (char-colon? $char) (char=? $char #\:))
  (define (char-left-parenthesis? $char) (char=? $char (char left-parenthesis)))

  (define-syntax (char $syntax)
    (syntax-case $syntax (code)
      ((_ (code i))
        (literal->syntax (integer->char (datum i))))
      ((_ id)
        (literal->syntax
          (case (datum id)
            ((colon) #\:)
            ((dot) #\.)
            ((comma) #\,)
            ((colon) #\:)
            ((semicolon) #\;)
            ((at-sign) #\@)
            ((vertical-bar) #\|)
            ((back-slash) #\\)
            ((hash-mark) #\#)
            ((quote) #\')
            ((back-tick) #\`)
            ((double-quote) #\")
            ((left-parenthesis) #\()
            ((right-parenthesis) #\))
            ((left-square-bracket) #\[)
            ((right-square-bracket) #\])
            ((0 1 2 3 4 5 6 7 8 9)
              (integer->char (+ (char->integer #\0) (datum id))))
            (else
              (read
                (open-input-string
                  (string-append
                    "#\\"
                    (symbol->string (datum id)))))))))))

  (define (char->datum $char)
    (let*
      (
        ($string (format "~s" $char))
        ($substring (substring $string 2 (string-length $string))))
      (case (string-ref $substring 0)
        ((#\x)
          `(code ,(string->number (string-append "#" $substring))))
        (else
          (case $substring
            ((".") 'dot)
            ((",") 'comma)
            ((":") 'colon)
            ((";") 'semicolon)
            (("@") 'at-sign)
            (("|") 'vertical-bar)
            (("\\") 'back-slash)
            (("#") 'hash-mark)
            (("'") 'quote)
            (("\"") 'double-quote)
            (("`") 'back-tick)
            (("(") 'left-parenthesis)
            ((")") 'right-parenthesis)
            (("[") 'left-square-bracket)
            (("]") 'right-square-bracket)
            (else
              (or
                (string->number $substring)
                (string->symbol $substring))))))))
)
