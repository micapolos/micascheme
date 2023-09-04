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

(check (equal? (parse (digit-parser) "") #f))
(check (equal? (parse (digit-parser) "0") 0))
(check (equal? (parse (digit-parser) "9") 9))
(check (equal? (parse (digit-parser) "10") #f))

; ---------------------------------------------------------

(check (equal? (parse (string-parser) "") ""))
(check (equal? (parse (string-parser) "$a1") "$a1"))

; ---------------------------------------------------------

(check (equal? (parse (exact-parser "") "") ""))
(check (equal? (parse (exact-parser "") "foo") #f))
(check (equal? (parse (exact-parser "foo") "fo") #f))
(check (equal? (parse (exact-parser "foo") "foo") "foo"))
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
  ($parser
    (parser-lets 
      ($number (positive-integer-parser))
      (parser (+ $number 1))))
  (check (equal? (parse $parser "12") 13)))

(lets
  ($parser
    (parser-lets 
      ($skip (exact-parser "- "))
      (positive-integer-parser)))
  (check (equal? (parse $parser "- 123") 123)))

(lets
  ($parser
    (parser-lets 
      ($number1 (positive-integer-parser))
      (skip (exact-parser " - "))
      ($number2 (positive-integer-parser))
      (parser (- $number1 $number2))))
  (check (equal? (parse $parser "3 - 2") 1)))

; ---------------------------------------------------------

(lets
  ($parser (make-parser "foo"))
  (check (equal? (parse $parser "foo") "foo")))

(lets
  ($parser (make-parser string))
  (check (equal? (parse $parser "foo") "foo")))

(lets
  ($parser (make-parser (stack char)))
  (check (equal? (parse $parser "foo") (stack #\f #\o #\o))))

(lets
  ($parser (make-parser (parsed 12.4)))
  (check (equal? (parse $parser "") 12.4)))

(lets
  ($parser 
    (make-parser 
      (lets
        ($number1 positive-integer)
        (skip "-")
        ($number2 positive-integer)
        (parsed (- $number1 $number2)))))
  (check (equal? (parse $parser "3-2") 1)))

; ---------------------------------------------------------

(begin
  (define-parser expr
    (lets
      ($number1 positive-integer)
      ($op
        (oneof 
          (lets (skip "+") (parsed +)) 
          (lets (skip "-") (parsed -))))
      ($number2 positive-integer)
      (parsed ($op $number1 $number2))))
  (begin
    (check (equal? (parse (make-parser expr) "3-2") 1))
    (check (equal? (parse (make-parser expr) "3+2") 5))))

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
