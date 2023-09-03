(import (micascheme) (leo parser))

(check (equal? (parse (parser-with "foo") "") "foo"))
(check (equal? (parse (parser-with "foo") "a") #f))
(check (equal? (parse (parser-with "foo") "foo") #f))

; ---------------------------------------------------------

(check (equal? (parse (char-parser) "") #f))
(check (equal? (parse (char-parser) "a") #\a))
(check (equal? (parse (char-parser) "1") #\1))
(check (equal? (parse (char-parser) "ab") #f))

; ---------------------------------------------------------

(check (equal? (parse (string-parser) "") ""))
(check (equal? (parse (string-parser) "$a1") "$a1"))

; ---------------------------------------------------------

(check (equal? (parse (exact-parser "") "") #t))
(check (equal? (parse (exact-parser "") "foo") #f))
(check (equal? (parse (exact-parser "foo") "fo") #f))
(check (equal? (parse (exact-parser "foo") "foo") #t))
(check (equal? (parse (exact-parser "foo") "foot") #f))

; ---------------------------------------------------------

(check (equal? (parse (line-parser) "") #f))
(check (equal? (parse (line-parser) "\n") ""))
(check (equal? (parse (line-parser) "foo") #f))
(check (equal? (parse (line-parser) "foo\n") "foo"))

; ---------------------------------------------------------

(check (equal? (parse (positive-integer-parser) "") #f))
(check (equal? (parse (positive-integer-parser) "012") 12))
(check (equal? (parse (positive-integer-parser) "012a") #f))
(check (equal? (parse (positive-integer-parser) "-012") #f))

; ---------------------------------------------------------

(lets
  ($parser
    (parser-bind (positive-integer-parser)
      (lambda ($positive-integer)
        (parser-bind (word-parser)
          (lambda ($word)
            (parser-with 
              (string-append
                (symbol->string $word)
                ": "
                (number->string $positive-integer))))))))
  (begin
    (check (equal? (parse $parser "") #f))
    (check (equal? (parse $parser "123") #f))
    (check (equal? (parse $parser "123m") "m: 123"))
    (check (equal? (parse $parser "123cm") "cm: 123"))
    (check (equal? (parse $parser "123!") #f))
    (check (equal? (parse $parser "123cm!") #f))))

; ---------------------------------------------------------

(lets
  ($parser (parser-map (line-parser) string-length))
  (begin
    (check (equal? (parse $parser "foo") #f))
    (check (equal? (parse $parser "foo\n") 3))
    (check (equal? (parse $parser "foo\nbar") #f))))

; ---------------------------------------------------------

(check (equal? (parse (word-parser) "") #f))
(check (equal? (parse (word-parser) "foo") `foo))
(check (equal? (parse (word-parser) "1") #f))
(check (equal? (parse (word-parser) "foo1") #f))

; ---------------------------------------------------------

(lets
  ($parser (oneof-parser (word-parser) (positive-integer-parser)))
  (begin
    (check (equal? (parse $parser "") #f))
    (check (equal? (parse $parser "foo") `foo))
    (check (equal? (parse $parser "123") 123))
    (check (equal? (parse $parser "$1") #f))))

; ---------------------------------------------------------

(lets
  ($parser (fold-parser (stack) (line-parser) push))
  (begin
    (check (equal? (parse $parser "") (stack)))
    (check (equal? (parse $parser "foo") #f))
    (check (equal? (parse $parser "foo\n") (stack "foo")))
    (check (equal? (parse $parser "foo\nbar") #f))
    (check (equal? (parse $parser "foo\nbar\n") (stack "foo" "bar")))))

; ---------------------------------------------------------

(lets
  ($parser (stack-parser (line-parser)))
  (begin
    (check (equal? (parse $parser "") (stack)))
    (check (equal? (parse $parser "foo") #f))
    (check (equal? (parse $parser "foo\n") (stack "foo")))
    (check (equal? (parse $parser "foo\nbar") #f))
    (check (equal? (parse $parser "foo\nbar\n") (stack "foo" "bar")))))

; ---------------------------------------------------------

(lets
  ($parser (indent-parser (string-parser)))
  (begin
    (check (equal? (parse $parser "") ""))
    (check (equal? (parse $parser " ") #f))
    (check (equal? (parse $parser "  ") #f))
    (check (equal? (parse $parser "  \n") "\n"))
    (check (equal? (parse $parser "  a") #f))
    (check (equal? (parse $parser "  a\n") "a\n"))))

; ---------------------------------------------------------
