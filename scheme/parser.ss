(library (scheme parser)
  (export
    identifier-parser)
  (import
    (micascheme)
    (parser))

  (define (identifier-parser)
    (oneof-parser
      (peculiar-identifier-parser)
      (lets
        ((parser $initial) (initial-parser))
        ((parser $subsequent-stack) (stack-parser (subsequent-parser)))
        (parser (string->symbol (list->string (cons $initial (reverse $subsequent-stack))))))))

  (define (peculiar-identifier-parser)
    (lets
      ((parser $string)
        (oneof-parser
          (exact-parser "+")
          (exact-parser "-")
          (exact-parser "...")))
      (parser (string->symbol $string))))

  (define (initial-parser)
    (oneof-parser
      (letter-parser)
      (special-initial-parser)))

  (define (special-initial-parser)
    (oneof-parser
      (exact-char-parser #\!)
      (exact-char-parser #\$)
      (exact-char-parser #\%)
      (exact-char-parser #\&)
      (exact-char-parser #\*)
      (exact-char-parser #\/)
      (exact-char-parser #\:)
      (exact-char-parser #\<)
      (exact-char-parser #\=)
      (exact-char-parser #\>)
      (exact-char-parser #\?)
      (exact-char-parser #\~)
      (exact-char-parser #\_)
      (exact-char-parser #\^)))

  (define (special-subsequent-parser)
    (oneof-parser
      (exact-char-parser #\+)
      (exact-char-parser #\-)
      (exact-char-parser #\.)))

  (define (subsequent-parser)
    (oneof-parser
      (initial-parser)
      (digit-char-parser)
      (special-subsequent-parser)))
)
