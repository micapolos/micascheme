(library (char)
  (export
    char->ascii
    char-newline?
    char-space?
    char-comma?
    char-dot?
    char-colon?
    char)
  (import
    (scheme)
    (syntax))

  (define (char->ascii $char)
    (bitwise-and #xff (char->integer $char)))

  (define (char-newline? $char) (char=? $char #\newline))
  (define (char-space? $char) (char=? $char #\space))
  (define (char-comma? $char) (char=? $char #\,))
  (define (char-dot? $char) (char=? $char #\.))
  (define (char-colon? $char) (char=? $char #\:))

  (define-syntax (char $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (literal->syntax
          (case (datum id)
            ((colon) #\:)
            ((dot) #\.)
            ((comma) #\,)
            ((at) #\@)
            ((0 1 2 3 4 5 6 7 8 9)
              (integer->char (+ (char->integer #\0) (datum id))))
            (else
              (read
                (open-input-string
                  (string-append
                    "#\\"
                    (symbol->string (datum id)))))))))))
)
